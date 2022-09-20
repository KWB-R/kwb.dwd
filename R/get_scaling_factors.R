#
# Ask DWD's documentation for the factor:
#
# urls <- kwb.dwd:::list_monthly_grids_germany_asc_gz("sunshine_duration")
# kwb.dwd:::open_description(urls[1L])
# kwb.dwd:::open_description(dirname(urls[1L])) # alternative
#
# or:
# urls <- kwb.dwd:::list_daily_grids_germany_tgz("soil_temperature_5cm")
# kwb.dwd:::open_description(urls[1L])
#

# get_scaling_factors ----------------------------------------------------------
get_scaling_factors <- function()
{
  list(
    # Monatsmittel des gemittelten taeglichen Lufttemperaturmaxima in 2 m Hoehe,
    # in 1/10 Grad Celsius
    air_temperature_max = 0.1,
    # Monatsmittel der gemittelten taeglichen Lufttemperatur in 2 m Hoehe, in
    # 1/10 Grad Celsius
    air_temperature_mean = 0.1,
    # Monatsmittel des gemittelten taeglichen Lufttemperaturminimums in 2 m
    # Hoehe, in 1/10 Grad Celsius
    air_temperature_min = 0.1,
    # Monatlicher Trockenheitsindex nach de Martonne, Einheit: mm/Grad Celsius
    drought_index = 1,
    # Die Werte im Raster muessen durch 10 dividiert werden, um die richtigen
    # Werte in mm zu bekommen
    evapo_p = 0.1,
    # Die Werte im Raster muessen durch 10 dividiert werden, um die richtigen
    # Werte in mm zu bekommen
    evapo_r = 0.1,
    # Die Werte sind in cm
    frost_depth = 1,
    # Monatssumme der Niederschlagshoehe in mm
    precipitation = 1,
    # Die Werte sind in Prozent pflanzenverfuegbares Wasser
    soil_moist = 0.1,
    # Die Werte im Raster muessen durch 10 dividiert werden, um die richtigen
    # Werte in Grad Celsius zu bekommen
    soil_temperature_5cm = 0.1,
    # Monatssumme der Sonnenscheindauer in h
    sunshine_duration = 1
  )
}
