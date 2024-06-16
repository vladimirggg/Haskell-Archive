main :: IO()
main = do
    print $ highestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Bulgaria"

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int
data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]


highestCapital :: [Country] -> Name
highestCapital countries = fst $ foldr (\ (Country name cap cities) acc@(country,elev) -> if getElevation cap cities > elev then (name, (getElevation cap cities)) else acc) ("",0) countries

getElevation :: Capital -> [City] -> Elevation
getElevation cap [] = error "No such capital in the city list!"
getElevation cap ((City name elevation _):cs) 
 | cap == name = elevation
 | otherwise = getElevation cap cs
