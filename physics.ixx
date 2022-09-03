export module physics;

import <numeric>;
import <ratio>;


namespace physics::units {

export template<typename T>
concept unit_value_type = requires(T value, std::intmax_t factor) 
{
  { value * factor } -> std::convertible_to<T>;
  { value / factor } -> std::convertible_to<T>;
};

export template <typename Unit_t, typename Type_of_unit, typename Value_t>
concept unit = std::constructible_from<Value_t> && unit_value_type<Value_t> && requires(Unit_t unit)
{
  { unit.value } -> std::convertible_to<Value_t>;
};

template<typename T, typename Value_t>
concept left_multipliable_value_type = requires(T lhs, Value_t rhs) 
{
  { lhs * rhs } -> std::convertible_to<Value_t>;
};

template<typename T, typename Value_t>
concept right_multipliable_value_type = requires(Value_t lhs, T rhs) 
{
  { lhs * rhs } -> std::convertible_to<Value_t>;
};

template<typename T, typename Value_t>
concept left_dividable_value_type = requires(T lhs, Value_t rhs) 
{
  { lhs / rhs } -> std::convertible_to<Value_t>;
};

template<typename T, typename Value_t>
concept right_dividable_value_type = requires(Value_t lhs, T rhs) 
{
  { lhs / rhs } -> std::convertible_to<Value_t>;
};


struct One_unit;

using Base_unit_t = std::ratio<1>;


template <typename... T>
struct Unit_tag_pack;


template <typename T>
struct Unit_tag_pack_from_tuple;

template <typename... T>
struct Unit_tag_pack_from_tuple<std::tuple<T...>>
{
  using Type = Unit_tag_pack<T...>;
};


template <typename H, typename... Ts>
struct Parameter_pack_list
{
  using Head = H;
  using Tail = std::tuple<Ts...>;
};


template <typename T, std::intmax_t power_value = 1>
struct Powered_unit_tag
{
  static constexpr auto power{ power_value };
};


template <typename Num, typename Den>
struct Fractioned_unit_tag;


template <typename T>
struct Rotated_fractioned_unit_tag;

template <typename... Num_pack, typename... Den_pack>
struct Rotated_fractioned_unit_tag<Fractioned_unit_tag<Unit_tag_pack<Num_pack...>, Unit_tag_pack<Den_pack...>>>
{
  using Type = Fractioned_unit_tag<Unit_tag_pack<Den_pack...>, Unit_tag_pack<Num_pack...>>;
};


template <typename T, typename U>
struct Are_powered_units_with_same_base : std::false_type
{
};

template <typename Base, std::intmax_t left_pow, std::intmax_t right_pow>
struct Are_powered_units_with_same_base<Powered_unit_tag<Base, left_pow>, Powered_unit_tag<Base, right_pow>>
  : std::true_type
{
};


template <typename Target, typename... Args>
struct Have_same_powered_unit
{
  static constexpr auto value{ (Are_powered_units_with_same_base<Target, Args>::value || ...) };
};


template <typename T, typename U>
struct Add_tuple_to_powered_unit;

template <typename T, typename... Right_pack>
struct Add_tuple_to_powered_unit<Powered_unit_tag<T, 0>, std::tuple<Right_pack...>>
{
  using Type = std::tuple<Right_pack...>;
};

template <typename T, typename... Right_pack>
struct Add_tuple_to_powered_unit<T, std::tuple<Right_pack...>>
{
  using Type = std::tuple<T, Right_pack...>;
};


template <typename Left, typename Right>
struct Multiply_powered_units
{
  using Type = Left;
};

template <typename Base, std::intmax_t left_pow, std::intmax_t right_pow>
struct Multiply_powered_units<Powered_unit_tag<Base, left_pow>, Powered_unit_tag<Base, right_pow>>
{
  using Type = Powered_unit_tag<Base, left_pow + right_pow>;
};


template <typename Left, typename Right>
struct Divided_powered_units
{
  using Type = Left;
};

template <typename Base, std::intmax_t left_pow, std::intmax_t right_pow>
struct Divided_powered_units<Powered_unit_tag<Base, left_pow>, Powered_unit_tag<Base, right_pow>>
{
  using Type = Powered_unit_tag<Base, left_pow - right_pow>;
};


template <typename T>
struct Single_powered_unit_tuple
{
  using Type = std::tuple<T>;
};

template <typename T>
struct Single_powered_unit_tuple<Powered_unit_tag<T, 0>>
{
  using Type = std::tuple<One_unit>;
};


template <typename Target, template <typename, typename> typename Action, typename T>
struct Action_on_same;

template <typename Target, template <typename, typename> typename Action, typename Arg>
struct Action_on_same<Target, Action, std::tuple<Arg>>
{
  using Current_t =
    std::conditional_t<Are_powered_units_with_same_base<Arg, Target>::value, typename Action<Arg, Target>::Type, Arg>;

  using Type = Single_powered_unit_tuple<Current_t>::Type;
};

template <typename Target, template <typename, typename> typename Action, typename... Args>
struct Action_on_same<Target, Action, std::tuple<Args...>>
{
  using List_t = Parameter_pack_list<Args...>;
  using Current_t = std::conditional_t<Are_powered_units_with_same_base<typename List_t::Head, Target>::value,
    typename Action<typename List_t::Head, Target>::Type,
    typename List_t::Head>;

  using Type =
    Add_tuple_to_powered_unit<Current_t, typename Action_on_same<Target, Action, typename List_t::Tail>::Type>::Type;
};


template <typename Left, typename Right>
struct Multiplication_helper_unit_tag;

template <typename T>
struct Multiplication_helper_unit_tag<T, One_unit>
{
  using Type = T;
};

template <typename... Left_den_pack, typename T>
requires(!std::same_as<T, One_unit>)
struct Multiplication_helper_unit_tag<Fractioned_unit_tag<Unit_tag_pack<One_unit>, Unit_tag_pack<Left_den_pack...>>, T>
{
  using Type = Fractioned_unit_tag<Unit_tag_pack<T>, Unit_tag_pack<Left_den_pack...>>;
};


template <typename... Left_num_pack, typename... Left_den_pack, typename T>
requires(!Have_same_powered_unit<T, Left_num_pack...>::value && !Have_same_powered_unit<T, Left_den_pack...>::value && !std::same_as<T, One_unit>) 
struct Multiplication_helper_unit_tag<Fractioned_unit_tag<Unit_tag_pack<Left_num_pack...>,
                                                             Unit_tag_pack<Left_den_pack...>>,
  T>
{
  using Type = Fractioned_unit_tag<Unit_tag_pack<Left_num_pack..., T>, Unit_tag_pack<Left_den_pack...>>;
};

template <typename... Left_num_pack, typename... Left_den_pack, typename T>
requires(Have_same_powered_unit<T, Left_num_pack...>::value && !Have_same_powered_unit<T, Left_den_pack...>::value && !std::same_as<T, One_unit>) 
struct Multiplication_helper_unit_tag<Fractioned_unit_tag<Unit_tag_pack<Left_num_pack...>,
                                                             Unit_tag_pack<Left_den_pack...>>,
  T>
{
  using Type = Fractioned_unit_tag<typename Unit_tag_pack_from_tuple<typename Action_on_same<T, Multiply_powered_units, std::tuple<Left_num_pack...>>::Type>::Type, Unit_tag_pack<Left_den_pack...>>;
};

template <typename... Left_num_pack, typename... Left_den_pack, typename T>
requires(!Have_same_powered_unit<T, Left_num_pack...>::value && Have_same_powered_unit<T, Left_den_pack...>::value) 
struct Multiplication_helper_unit_tag<Fractioned_unit_tag<Unit_tag_pack<Left_num_pack...>,
                                                             Unit_tag_pack<Left_den_pack...>>,
  T>
{
  using Type = Fractioned_unit_tag<Unit_tag_pack<Left_num_pack...>, typename Unit_tag_pack_from_tuple<typename Action_on_same<T, Divided_powered_units, std::tuple<Left_den_pack...>>::Type>::Type>;
};


template <typename Left, typename Right>
struct Multiply_all_unit_tag;

template <typename Left_t, typename T>
struct Multiply_all_unit_tag<Left_t, std::tuple<T>>
{
  using Type = Multiplication_helper_unit_tag<Left_t, T>::Type;
};

template <typename Left_t, typename... Right_num_pack>
struct Multiply_all_unit_tag<Left_t, std::tuple<Right_num_pack...>>
{
  using List = Parameter_pack_list<Right_num_pack...>;
  using Type = Multiply_all_unit_tag<typename Multiplication_helper_unit_tag<Left_t, typename List::Head>::Type,
    typename List::Tail>::Type;
};


template <typename Left, typename Right>
struct Multiplication_unit_tag;

template <typename Left_t, typename... Right_num_pack, typename... Right_den_pack>
struct Multiplication_unit_tag<Left_t,
  Fractioned_unit_tag<Unit_tag_pack<Right_num_pack...>, Unit_tag_pack<Right_den_pack...>>>
{
  using After_multiplication_t = Multiply_all_unit_tag<Left_t, std::tuple<Right_num_pack...>>::Type;
  using Rotated_after_division_t =
    Multiply_all_unit_tag<typename Rotated_fractioned_unit_tag<After_multiplication_t>::Type,
      std::tuple<Right_den_pack...>>::Type;

  using Type = Rotated_fractioned_unit_tag<Rotated_after_division_t>::Type;
};


template <typename Left, typename Right>
struct Division_unit_tag
{
  using Type = Multiplication_unit_tag<Left, typename Rotated_fractioned_unit_tag<Right>::Type>::Type;
};


export template <typename Type_of_unit, unit_value_type Value_t, typename Ratio_t = Base_unit_t>
struct Unit
{
  using Type = Type_of_unit;

  explicit constexpr Unit(std::convertible_to<Value_t> auto value) : value{ static_cast<Value_t>(value) } {}

  constexpr auto operator<=>(const Unit&) const = default;

  template <std::convertible_to<Value_t> Other_value_t>
  constexpr operator Unit<Type_of_unit, Other_value_t, Ratio_t>() const
  {
    return Unit<Type_of_unit, Value_t, Ratio_t>{ static_cast<Other_value_t>(value) };
  }

  template <std::convertible_to<Value_t> Other_value_t, typename New_ratio_t>
  constexpr operator Unit<Type_of_unit, Other_value_t, New_ratio_t>() const
  {
    using div_ratio = std::ratio_divide<Ratio_t, New_ratio_t>;

    return Unit<Type_of_unit, Other_value_t, New_ratio_t>{ static_cast<Other_value_t>(
      (value * div_ratio::num) / div_ratio::den) };
  }

  constexpr auto to_base() { return static_cast<Unit<Type_of_unit, Value_t, Base_unit_t>>(*this); }

  Value_t value;
};

export template <typename First_tag_t,
  unit_value_type First_value_t,
  typename First_ratio_t,
  typename Second_tag_t,
  unit_value_type Second_value_t,
  typename Second_ratio_t>
constexpr auto operator*(Unit<First_tag_t, First_value_t, First_ratio_t> lhs,
  Unit<Second_tag_t, Second_value_t, Second_ratio_t> rhs)
{
  const auto result_value{ lhs.value * rhs.value };

  using Result_value_t = std::remove_reference_t<decltype(result_value)>;
  using Result_ratio_t = std::ratio_multiply<First_ratio_t, Second_ratio_t>;

  return Unit<typename Multiplication_unit_tag<First_tag_t, Second_tag_t>::Type, Result_value_t, Result_ratio_t>{
    result_value
  };
}

export template <typename First_tag_t,
  unit_value_type First_value_t,
  typename First_ratio_t,
  typename Second_tag_t,
  unit_value_type Second_value_t,
  typename Second_ratio_t>
constexpr auto operator/(Unit<First_tag_t, First_value_t, First_ratio_t> lhs,
  Unit<Second_tag_t, Second_value_t, Second_ratio_t> rhs)
{
  const auto result_value{ lhs.value / rhs.value };

  using Result_value_t = std::remove_reference_t<decltype(result_value)>;
  using Result_ratio_t = std::ratio_divide<First_ratio_t, Second_ratio_t>;

  return Unit<typename Division_unit_tag<First_tag_t, Second_tag_t>::Type, Result_value_t, Result_ratio_t>{
    result_value
  };
}

export template <typename Tag_t, unit_value_type Value_t, typename Ratio_t>
constexpr auto operator*(Unit<Tag_t, Value_t, Ratio_t> lhs, right_multipliable_value_type<Value_t> auto rhs)
{
  return Unit<Tag_t, Value_t, Ratio_t>{ lhs.value * rhs };
}

export template <typename Tag_t, unit_value_type Value_t, typename Ratio_t>
constexpr auto operator/(Unit<Tag_t, Value_t, Ratio_t> lhs, right_dividable_value_type<Value_t> auto rhs)
{
  return Unit<Tag_t, Value_t, Ratio_t>{ lhs.value / rhs };
}

export template <typename Tag_t, unit_value_type Value_t, typename Ratio_t>
constexpr auto operator*(left_multipliable_value_type<Value_t> auto lhs, Unit<Tag_t, Value_t, Ratio_t> rhs)
{
  return Unit<Tag_t, Value_t, Ratio_t>{ lhs * rhs.value };
}
export template <typename Tag_t, unit_value_type Value_t, typename Ratio_t>
constexpr auto operator/(left_dividable_value_type<Value_t> auto lhs, Unit<Tag_t, Value_t, Ratio_t> rhs)
{
  return Unit<Tag_t, Value_t, Ratio_t>{ lhs * rhs.value };
}


template <typename First_t, typename Second_t>
struct Contains_same_elements : std::false_type
{
};

template <>
struct Contains_same_elements<Unit_tag_pack<One_unit>, Unit_tag_pack<One_unit>> : std::true_type
{
};

template <typename... First_pack, typename... Second_pack>
struct Contains_same_elements<Unit_tag_pack<First_pack...>, Unit_tag_pack<Second_pack...>>
  : std::integral_constant<bool,
      sizeof...(First_pack) == sizeof...(Second_pack)
        && std::conjunction_v<Have_same_powered_unit<First_pack, Second_pack...>...>>
{
};


template <typename First_t, typename Second_t>
struct Are_tags_same : std::false_type
{
};

template <typename First_num_pack, typename First_den_pack, typename Second_num_pack, typename Second_den_pack>
struct Are_tags_same<Fractioned_unit_tag<First_num_pack, First_den_pack>,
  Fractioned_unit_tag<Second_num_pack, Second_den_pack>>
  : std::integral_constant<bool,
      Contains_same_elements<First_num_pack, Second_num_pack>::value
        && Contains_same_elements<First_den_pack, Second_den_pack>::value>
{
};


export template <typename First_t, typename Second_t>
struct Is_same : std::false_type
{};

export template <typename First_tag_t,
  unit_value_type First_value_t,
  typename First_ratio_t,
  typename Second_tag_t,
  unit_value_type Second_value_t,
  typename Second_ratio_t>
struct Is_same<Unit<First_tag_t, First_value_t, First_ratio_t>, Unit<Second_tag_t, Second_value_t, Second_ratio_t>>
  : std::integral_constant<bool, Are_tags_same<First_tag_t, Second_tag_t>::value>
{
};


template <typename Base_unit_tag>
using New_unit_tag = Fractioned_unit_tag<Unit_tag_pack<Powered_unit_tag<Base_unit_tag>>, Unit_tag_pack<One_unit>>;


struct Length_unit_tag;

export template <unit_value_type Value_t, typename Ratio_t = Base_unit_t>
using Length = Unit<New_unit_tag<Length_unit_tag>, Value_t, Ratio_t>;

export template <unit_value_type Value_t>
using Meters = Length<Value_t>;

export template <unit_value_type Value_t>
using Kilometers = Length<Value_t, std::kilo>;

export template <unit_value_type Value_t>
using Centimeters = Length<Value_t, std::centi>;

export template <unit_value_type Value_t>
using Decimeters = Length<Value_t, std::deci>;

export template <unit_value_type Value_t>
using Milimeters = Length<Value_t, std::milli>;


struct Mass_unit_tag;

export template <unit_value_type Value_t, typename Ratio_t = Base_unit_t>
using Mass = Unit<New_unit_tag<Mass_unit_tag>, Value_t, Ratio_t>;

export template <unit_value_type Value_t>
using Kilograms = Mass<Value_t>;

export template <unit_value_type Value_t>
using Grams = Mass<Value_t, std::milli>;

struct Time_unit_tag;

export template <unit_value_type Value_t, typename Ratio_t = Base_unit_t>
using Time = Unit<New_unit_tag<Time_unit_tag>, Value_t, Ratio_t>;

export template <unit_value_type Value_t>
using Seconds = Time<Value_t>;

export template <unit_value_type Value_t>
using Hours = Time<Value_t, std::ratio<3600>>;


export template <unit_value_type Value_t, typename Ratio_t = Base_unit_t>
using Velocity =
  Unit<typename Division_unit_tag<typename Length<Value_t, Ratio_t>::Type, typename Time<Value_t, Ratio_t>::Type>::Type,
    Value_t,
    Ratio_t>;

export template <unit_value_type Value_t, typename Ratio_t = Base_unit_t>
using Acceleration = Unit<
  typename Division_unit_tag<typename Velocity<Value_t, Ratio_t>::Type, typename Time<Value_t, Ratio_t>::Type>::Type,
  Value_t,
  Ratio_t>;


using num_base_t = long double;
export namespace literals {
  constexpr auto operator"" _m(num_base_t value) { return Meters<num_base_t>{ value }; }
  constexpr auto operator"" _km(num_base_t value) { return Kilometers<num_base_t>{ value }; }
  constexpr auto operator"" _cm(num_base_t value) { return Centimeters<num_base_t>{ value }; }
  constexpr auto operator"" _dm(num_base_t value) { return Decimeters<num_base_t>{ value }; }
  constexpr auto operator"" _mm(num_base_t value) { return Milimeters<num_base_t>{ value }; }

  constexpr auto operator"" _kg(num_base_t value) { return Kilograms<num_base_t>{ value }; }
  constexpr auto operator"" _g(num_base_t value) { return Grams<num_base_t>{ value }; }

  constexpr auto operator"" _s(num_base_t value) { return Seconds<num_base_t>{ value }; }
  constexpr auto operator"" _h(num_base_t value) { return Hours<num_base_t>{ value }; }
}// namespace literals

}// namespace physics::units


export namespace physics::constants {
template <std::floating_point T>
inline static constexpr units::Acceleration<T> gravitational_acceleration{ static_cast<T>(9.80665) };
}
