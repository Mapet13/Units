# Units
Physical unit type system made created for modern C++ template metaprogramming learning  

### Goal
Main goal was to create type system where you can easly compare unit types. Unfortunetly I didn't achive that 100% becouse after many operations resulted type may have different order of arguments. But it actualy has the simplest form so I've partialy succesed. 

## Examples
```
// literals and math operation example
const auto time{ 10.0_s };
const auto distance{ 120.0_m };
const auto velocity{ distance / time };
const auto acceleration{ velocity / time };
const auto velocity_2{ acceleration * time };

// comparing types
static_assert(Is_same<decltype(time * distance), decltype(distance * time)>::value);
static_assert(!Is_same<decltype(time * distance), decltype(distance / time)>::value);

// example of physical constant made with units
template <std::floating_point T>
inline static constexpr units::Acceleration<T> gravitational_acceleration{ static_cast<T>(9.80665) };
```
