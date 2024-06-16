main :: IO()
main = do
    print $ coldestCapital [
                            (Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), 
                            (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), 
                            (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Germany" -- Germany and France are with the same avg (14.0)

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]

coldestCapital :: [Country] -> Name
coldestCapital countries = fst $ foldl1 (\(name1, avgTemp1) (name2, avgTemp2) -> if avgTemp1 <= avgTemp2 then (name1, avgTemp1) else (name2, avgTemp2)) countryTemps
  where
    countryTemps = [(name, avgYearlyTemp) | Country name _ cities <- countries, let avgYearlyTemp = averageYearlyTemperature cities]

averageYearlyTemperature :: [City] -> AvgYearlyTemperature
averageYearlyTemperature cities = sum yearlyTemps / fromIntegral (length yearlyTemps)
  where yearlyTemps = [temp | City _ _ temp <- cities]