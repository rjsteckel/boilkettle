
weather_forecast <- function(zipcode) {
  set_api_key(Sys.getenv('WUNDERGROUND_KEY'))
  forecast3day(set_location(zip_code=zipcode))
}
