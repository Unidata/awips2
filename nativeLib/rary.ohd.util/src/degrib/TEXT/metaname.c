/*****************************************************************************
 * metaname.c
 *
 * DESCRIPTION
 *    This file contains the code necessary to parse the GRIB2 product
 * definition information into human readable text.  In addition to the
 * tables in the GRIB2 specs, it also attempts to handle local table
 * definitions that NCEP and NDFD have developed.
 *
 * HISTORY
 *    1/2004 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
#include <string.h>
#include <stdlib.h>
#include "degrib_inc/meta.h"
#include "degrib_inc/metaname.h"
#include "degrib_inc/myerror.h"
#include "degrib_inc/myassert.h"
#include "degrib_inc/myutil.h"

typedef struct {
   char *name, *comment, *unit;
   unit_convert convert;
} GRIB2ParmTable;

typedef struct {
   int prodType, cat, subcat;
   char *name, *comment, *unit;
   unit_convert convert;
} GRIB2LocalTable;

typedef struct {
   char *GRIB2name, *NDFDname;
} NDFD_AbrevOverideTable;

typedef struct {
   char *name, *comment, *unit;
} GRIB2SurfTable;

/* *INDENT-OFF* */
/* GRIB2 Code table 4.2 : 0.0 */
GRIB2ParmTable MeteoTemp[] = {
   /* 0 */ {"TMP", "Temperature", "K", UC_K2F},  /* Need NDFD override. T */
   /* 1 */ {"VTMP", "Virtual temperature", "K", UC_K2F},
   /* 2 */ {"POT", "Potential temperature", "K", UC_K2F},
   /* 3 */ {"EPOT", "Pseudo-adiabatic potential temperature", "K", UC_K2F},
   /* 4 */ {"TMAX", "Maximum Temperature", "K", UC_K2F}, /* Need NDFD override MaxT */
   /* 5 */ {"TMIN", "Minimum Temperature", "K", UC_K2F}, /* Need NDFD override MinT */
   /* 6 */ {"DPT", "Dew point temperature", "K", UC_K2F}, /* Need NDFD override Td */
   /* 7 */ {"DEPR", "Dew point depression", "K", UC_K2F},
   /* 8 */ {"LAPR", "Lapse rate", "K/m", UC_NONE},
   /* 9 */ {"TMPA", "Temperature anomaly", "K", UC_K2F},
   /* 10 */ {"LHTFL", "Latent heat net flux", "W/(m^2)", UC_NONE},
   /* 11 */ {"SHTFL", "Sensible heat net flux", "W/(m^2)", UC_NONE},
            /* NDFD */
   /* 12 */ {"HeatIndex", "Heat index", "K", UC_K2F},
            /* NDFD */
   /* 13 */ {"WCI", "Wind chill factor", "K", UC_K2F},
   /* 14 */ {"", "Minimum dew point depression", "K", UC_K2F},
   /* 15 */ {"VPTMP", "Virtual potential temperature", "K", UC_K2F},
};

/* GRIB2 Code table 4.2 : 0.1 */
/* NCEP added "Water" to items 22, 24, 25 */
GRIB2ParmTable MeteoMoist[] = {
   /* 0 */ {"SPFH", "Specific humidity", "kg/kg", UC_NONE},
   /* 1 */ {"RH", "Relative Humidity", "%", UC_NONE},
   /* 2 */ {"MIXR", "Humidity mixing ratio", "kg/kg", UC_NONE},
   /* 3 */ {"PWAT", "Precipitable water", "kg/(m^2)", UC_NONE},
   /* 4 */ {"VAPP", "Vapor Pressure", "Pa", UC_NONE},
   /* 5 */ {"SATD", "Saturation deficit", "Pa", UC_NONE},
   /* 6 */ {"EVP", "Evaporation", "kg/(m^2)", UC_NONE},
   /* 7 */ {"PRATE", "Precipitation rate", "kg/(m^2 s)", UC_NONE},
   /* 8 */ {"APCP", "Total precipitation", "kg/(m^2)", UC_InchWater}, /* Need NDFD override QPF */
   /* 9 */ {"NCPCP", "Large scale precipitation", "kg/(m^2)", UC_NONE},
   /* 10 */ {"ACPCP", "Convective precipitation", "kg/(m^2)", UC_NONE},
   /* 11 */ {"SNOD", "Snow depth", "m", UC_M2Inch}, /* Need NDFD override SnowDepth */
   /* 12 */ {"SRWEQ", "Snowfall rate water equivalent", "kg/(m^2 s)", UC_NONE},
   /* 13 */ {"WEASD", "Water equivalent of accumulated snow depth",
             "kg/(m^2)", UC_NONE},
   /* 14 */ {"SNOC", "Convective snow", "kg/(m^2)", UC_NONE},
   /* 15 */ {"SNOL", "Large scale snow", "kg/(m^2)", UC_NONE},
   /* 16 */ {"SNOM", "Snow melt", "kg/(m^2)", UC_NONE},
   /* 17 */ {"", "Snow age", "day", UC_NONE},
   /* 18 */ {"", "Absolute humidity", "kg/(m^3)", UC_NONE},
   /* 19 */ {"", "Precipitation type", "(1 Rain, 2 Thunderstorm, "
             "3 Freezing Rain, 4 Mixed/ice, 5 snow, 255 missing)", UC_NONE},
   /* 20 */ {"", "Integrated liquid water", "kg/(m^2)", UC_NONE},
   /* 21 */ {"", "Condensate", "kg/kg", UC_NONE},
   /* 22 */ {"CLWMR", "Cloud Water Mixing Ratio", "kg/kg", UC_NONE},
   /* 23 */ {"", "Ice water mixing ratio", "kg/kg", UC_NONE},   /* ICMR? */
   /* 24 */ {"RWMR", "Rain Water Mixing Ratio", "kg/kg", UC_NONE},
   /* 25 */ {"SNMR", "Snow Water Mixing Ratio", "kg/kg", UC_NONE},
   /* 26 */ {"", "Horizontal moisture convergence", "kg/(kg s)", UC_NONE},
   /* 27 */ {"", "Maximum relative humidity", "%", UC_NONE},
   /* 28 */ {"", "Maximum absolute humidity", "kg/(m^3)", UC_NONE},
            /* NDFD */
   /* 29 */ {"SnowAmt", "Total snowfall", "m", UC_M2Inch},
   /* 30 */ {"", "Precipitable water category", "(undefined)", UC_NONE},
   /* 31 */ {"", "Hail", "m", UC_NONE},
   /* 32 */ {"", "Graupel (snow pellets)", "kg/kg", UC_NONE},
};

/* GRIB2 Code table 4.2 : 0.2 */
GRIB2ParmTable MeteoMoment[] = {
   /* 0 */ {"WDIR", "Wind direction (from which blowing)", "deg true",
            UC_NONE}, /* Need NDFD override WindDir */
   /* 1 */ {"WIND", "Wind speed", "m/s", UC_MS2Knots}, /* Need NDFD override WindSpd */
   /* 2 */ {"UGRD", "u-component of wind", "m/s", UC_NONE},
   /* 3 */ {"VGRD", "v-component of wind", "m/s", UC_NONE},
   /* 4 */ {"STRM", "Stream function", "(m^2)/s", UC_NONE},
   /* 5 */ {"VPOT", "Velocity potential", "(m^2)/s", UC_NONE},
   /* 6 */ {"MNTSF", "Montgomery stream function", "(m^2)/(s^2)", UC_NONE},
   /* 7 */ {"SGCVV", "Sigma coordinate vertical velocity", "1/s", UC_NONE},
   /* 8 */ {"VVEL", "Vertical velocity (pressure)", "Pa/s", UC_NONE}, /* NCEP override WEL?  */
   /* 9 */ {"DZDT", "Verical velocity (geometric)", "m/s", UC_NONE},
   /* 10 */ {"ABSV", "Absolute vorticity", "1/s", UC_NONE},
   /* 11 */ {"ABSD", "Absolute divergence", "1/s", UC_NONE},
   /* 12 */ {"RELV", "Relative vorticity", "1/s", UC_NONE},
   /* 13 */ {"RELD", "Relative divergence", "1/s", UC_NONE},
   /* 14 */ {"PVORT", "Potential vorticity", "K(m^2)/(kg s)", UC_NONE},
   /* 15 */ {"VUCSH", "Vertical u-component shear", "1/s", UC_NONE},
   /* 16 */ {"VVCSH", "Vertical v-component shear", "1/s", UC_NONE},
   /* 17 */ {"UFLX", "Momentum flux, u component", "N/(m^2)", UC_NONE},
   /* 18 */ {"VFLX", "Momentum flux, v component", "N/(m^2)", UC_NONE},
   /* 19 */ {"WMIXE", "Wind mixing energy", "J", UC_NONE},
   /* 20 */ {"BLYDP", "Boundary layer dissipation", "W/(m^2)", UC_NONE},
   /* 21 */ {"", "Maximum wind speed", "m/s", UC_NONE},
   /* 22 */ {"", "Wind speed (gust)", "m/s", UC_NONE},  /* GUST? */
   /* 23 */ {"", "u-component of wind (gust)", "m/s", UC_NONE},
   /* 24 */ {"", "v-component of wind (gust)", "m/s", UC_NONE},
};

/* GRIB2 Code table 4.2 : 0.3 */
GRIB2ParmTable MeteoMass[] = {
   /* 0 */ {"PRES", "Pressure", "Pa", UC_NONE},
   /* 1 */ {"PRMSL", "Pressure reduced to MSL", "Pa", UC_NONE},
   /* 2 */ {"PTEND", "Pressure tendency", "Pa/s", UC_NONE},
   /* 3 */ {"ICAHT", "ICAO Standard Atmosphere Reference Height", "m", UC_NONE},
   /* 4 */ {"GP", "Geopotential", "(m^2)/(s^2)", UC_NONE},
   /* 5 */ {"HGT", "Geopotential height", "gpm", UC_NONE},
   /* 6 */ {"DIST", "Geometric Height", "m", UC_NONE},
   /* 7 */ {"HSTDV", "Standard deviation of height", "m", UC_NONE},
   /* 8 */ {"PRESA", "Pressure anomaly", "Pa", UC_NONE},
   /* 9 */ {"GPA", "Geopotential height anomally", "gpm", UC_NONE},
   /* 10 */ {"DEN", "Density", "kg/(m^3)", UC_NONE},
   /* 11 */ {"", "Altimeter setting", "Pa", UC_NONE},
   /* 12 */ {"", "Thickness", "m", UC_NONE},
   /* 13 */ {"", "Pressure altitude", "m", UC_NONE},
   /* 14 */ {"", "Density altitude", "m", UC_NONE},
};

/* GRIB2 Code table 4.2 : 0.4 */
GRIB2ParmTable MeteoShortRadiate[] = {
   /* 0 */ {"NSWRS", "Net short-wave radiation flux (surface)", "W/(m^2)", UC_NONE},
   /* 1 */ {"NSWRT", "Net short-wave radiation flux (top of atmosphere)",
            "W/(m^2)", UC_NONE},
   /* 2 */ {"SWAVR", "Short wave radiation flux", "W/(m^2)", UC_NONE},
   /* 3 */ {"GRAD", "Global radiation flux", "W/(m^2)", UC_NONE},
   /* 4 */ {"BRTMP", "Brightness temperature", "K", UC_NONE},
   /* 5 */ {"LWRAD", "Radiance (with respect to wave number)", "W/(m sr)", UC_NONE},
   /* 6 */ {"SWRAD", "Radiance (with respect to wave length)", "W/(m^3 sr)", UC_NONE},
};

/* GRIB2 Code table 4.2 : 0.5 */
GRIB2ParmTable MeteoLongRadiate[] = {
   /* 0 */ {"NLWRS", "Net long wave radiation flux (surface)", "W/(m^2)", UC_NONE},
   /* 1 */ {"NLWRT", "Net long wave radiation flux (top of atmosphere)",
            "W/(m^2)", UC_NONE},
   /* 2 */ {"LWAVR", "Long wave radiation flux", "W/(m^2)", UC_NONE},
};

/* GRIB2 Code table 4.2 : 0.6 */
GRIB2ParmTable MeteoCloud[] = {
   /* 0 */ {"CICE", "Cloud Ice", "kg/(m^2)", UC_NONE},
   /* 1 */ {"TCDC", "Total cloud cover", "%", UC_NONE}, /* Need NDFD override Sky */
   /* 2 */ {"CDCON", "Convective cloud cover", "%", UC_NONE},
   /* 3 */ {"LCDC", "Low cloud cover", "%", UC_NONE},
   /* 4 */ {"MCDC", "Medium cloud cover", "%", UC_NONE},
   /* 5 */ {"HCDC", "High cloud cover", "%", UC_NONE},
   /* 6 */ {"CWAT", "Cloud water", "kg/(m^2)", UC_NONE},
   /* 7 */ {"", "Cloud amount", "%", UC_NONE},
   /* 8 */ {"", "Cloud type", "(0 clear, 1 Cumulonimbus, 2 Stratus, "
            "3 Stratocumulus, 4 Cumulus, 5 Altostratus, 6 Nimbostratus, "
            "7 Altocumulus, 8 Cirrostratus, 9 Cirrocumulus, 10 Cirrus, "
            "11 Cumulonimbus (fog), 12 Stratus (fog), 13 Stratocumulus (fog),"
            " 14 Cumulus (fog), 15 Altostratus (fog), 16 Nimbostratus (fog), "
            "17 Altocumulus (fog), 18 Cirrostratus (fog), "
            "19 Cirrocumulus (fog), 20 Cirrus (fog), 191 unknown, "
            "255 missing)", UC_NONE},
   /* 9 */ {"", "Thunderstorm maximum tops", "m", UC_NONE},
   /* 10 */ {"", "Thunderstorm coverage", "(0 none, 1 isolated (1%-2%), "
             "2 few (3%-15%), 3 scattered (16%-45%), 4 numerous (> 45%), "
             "255 missing)", UC_NONE},
   /* 11 */ {"", "Cloud base", "m", UC_NONE},
   /* 12 */ {"", "Cloud top", "m", UC_NONE},
   /* 13 */ {"", "Ceiling", "m", UC_NONE},
};

/* GRIB2 Code table 4.2 : 0.7 */
/* NCEP capitalized items 6, 7, 8 */
GRIB2ParmTable MeteoStability[] = {
   /* 0 */ {"PLI", "Parcel lifted index (to 500 hPa)", "K", UC_NONE},
   /* 1 */ {"BLI", "Best lifted index (to 500 hPa)", "K", UC_NONE},
   /* 2 */ {"KX", "K index", "K", UC_NONE},
   /* 3 */ {"", "KO index", "K", UC_NONE},
   /* 4 */ {"", "Total totals index", "K", UC_NONE},
   /* 5 */ {"SX", "Sweat index", "numeric", UC_NONE},
   /* 6 */ {"CAPE", "Convective Available Potential Energy", "J/kg", UC_NONE},
   /* 7 */ {"CIN", "Convective Inhibition", "J/kg", UC_NONE},
   /* 8 */ {"HLCY", "Storm Relative Helicity", "J/kg", UC_NONE},
   /* 9 */ {"", "Energy helicity index", "numeric", UC_NONE},
};

/* GRIB2 Code table 4.2 : 0.13 */
GRIB2ParmTable MeteoAerosols[] = {
   /* 0 */ {"", "Aerosol type", "(0 Aerosol not present, 1 Aerosol present, "
            "255 missing)", UC_NONE},
};

/* GRIB2 Code table 4.2 : 0.14 */
GRIB2ParmTable MeteoGases[] = {
   /* 0 */ {"TOZNE", "Total ozone", "Dobson", UC_NONE},
};

/* GRIB2 Code table 4.2 : 0.15 */
GRIB2ParmTable MeteoRadar[] = {
   /* 0 */ {"", "Base spectrum width", "m/s", UC_NONE},
   /* 1 */ {"", "Base reflectivity", "dB", UC_NONE},
   /* 2 */ {"", "Base radial velocity", "m/s", UC_NONE},
   /* 3 */ {"", "Vertically-integrated liquid", "kg/m", UC_NONE},
   /* 4 */ {"", "Layer-maximum base reflectivity", "dB", UC_NONE},
   /* 5 */ {"", "Precipitation", "kg/(m^2)", UC_NONE},
   /* 6 */ {"RDSP1", "Radar spectra (1)", "-", UC_NONE},
   /* 7 */ {"RDSP2", "Radar spectra (2)", "-", UC_NONE},
   /* 8 */ {"RDSP3", "Radar spectra (3)", "-", UC_NONE},
};

/* GRIB2 Code table 4.2 : 0.18 */
GRIB2ParmTable MeteoNuclear[] = {
   /* 0 */ {"", "Air concentration of Caesium 137", "Bq/(m^3)", UC_NONE},
   /* 1 */ {"", "Air concentration of Iodine 131", "Bq/(m^3)", UC_NONE},
   /* 2 */ {"", "Air concentration of radioactive pollutant", "Bq/(m^3)", UC_NONE},
   /* 3 */ {"", "Ground deposition of Caesium 137", "Bq/(m^2)", UC_NONE},
   /* 4 */ {"", "Ground deposition of Iodine 131", "Bq/(m^2)", UC_NONE},
   /* 5 */ {"", "Ground deposition of radioactive pollutant", "Bq/(m^2)", UC_NONE},
   /* 6 */ {"", "Time-integrated air concentration of caesium pollutant",
            "(Bq s)/(m^3)", UC_NONE},
   /* 7 */ {"", "Time-integrated air concentration of iodine pollutant",
            "(Bq s)/(m^3)", UC_NONE},
   /* 8 */ {"", "Time-integrated air concentration of radioactive pollutant",
            "(Bq s)/(m^3)", UC_NONE},
};

/* GRIB2 Code table 4.2 : 0.19 */
/* NCEP capitalized items 11 */
GRIB2ParmTable MeteoAtmos[] = {
   /* 0 */ {"VIS", "Visibility", "m", UC_NONE},
   /* 1 */ {"ALBDO", "Albedo", "%", UC_NONE},
   /* 2 */ {"TSTM", "Thunderstorm probability", "%", UC_NONE},
   /* 3 */ {"MIXHT", "mixed layer depth", "m", UC_NONE},
   /* 4 */ {"", "Volcanic ash", "(0 not present, 1 present, 255 missing)", UC_NONE},
   /* 5 */ {"", "Icing top", "m", UC_NONE},
   /* 6 */ {"", "Icing base", "m", UC_NONE},
   /* 7 */ {"", "Icing", "(0 None, 1 Light, 2 Moderate, 3 Severe, "
            "255 missing)", UC_NONE},
   /* 8 */ {"", "Turbulance top", "m", UC_NONE},
   /* 9 */ {"", "Turbulence base", "m", UC_NONE},
   /* 10 */ {"", "Turbulance", "(0 None(smooth), 1 Light, 2 Moderate, "
             "3 Severe, 4 Extreme, 255 missing)", UC_NONE},
   /* 11 */ {"TKE", "Turbulent Kinetic Energy", "J/kg", UC_NONE},
   /* 12 */ {"", "Planetary boundary layer regime", "(0 Reserved, 1 Stable, "
             "2 Mechanically driven turbulence, 3 Forced convection, "
             "4 Free convection, 255 missing)", UC_NONE},
   /* 13 */ {"", "Contrail intensity", "(0 Contrail not present, "
             "1 Contrail present, 255 missing)", UC_NONE},
   /* 14 */ {"", "Contrail engine type", "(0 Low bypass, 1 High bypass, "
             "2 Non bypass, 255 missing)", UC_NONE},
   /* 15 */ {"", "Contrail top", "m", UC_NONE},
   /* 16 */ {"", "Contrail base", "m", UC_NONE},
};

/* GRIB2 Code table 4.2 : 0.253 or 0.190 (Document is inconsistant.) */
GRIB2ParmTable MeteoText[] = {
   /* 0 */ {"", "Arbitrary text string", "CCITTIA5", UC_NONE},
};

/* GRIB2 Code table 4.2 : 1.0 */
GRIB2ParmTable HydroBasic[] = {
   /* 0 */ {"", "Flash flood guidance", "kg/(m^2)", UC_NONE},
   /* 1 */ {"", "Flash flood runoff", "kg/(m^2)", UC_NONE},
   /* 2 */ {"", "Remotely sensed snow cover", "(50 no-snow/no-cloud, "
            "100 Clouds, 250 Snow, 255 missing)", UC_NONE},
   /* 3 */ {"", "Elevation of snow covered terrain", "(0-90 elevation in "
            "increments of 100m, 254 clouds, 255 missing)", UC_NONE},
   /* 4 */ {"", "Snow water equivalent percent of normal", "%", UC_NONE},
};

/* GRIB2 Code table 4.2 : 1.1 */
GRIB2ParmTable HydroProb[] = {
   /* 0 */ {"", "Conditional percent precipitation amount fractile for an "
            "overall period", "kg/(m^2)", UC_NONE},
   /* 1 */ {"", "Percent precipitation in a sub-period of an overall period",
            "%", UC_NONE},
   /* 2 */ {"", "Probability of 0.01 inch of precipitation (POP)", "%", UC_NONE},
};

/* GRIB2 Code table 4.2 : 2.0 */
GRIB2ParmTable LandVeg[] = {
   /* 0 */ {"LAND", "Land cover (1=land, 2=sea)", "Proportion", UC_NONE},
   /* 1 */ {"SFCR", "Surface roughness", "m", UC_NONE}, /*NCEP override SFRC? */
   /* 2 */ {"TSOIL", "Soil temperature", "K", UC_NONE},
   /* 3 */ {"SOILM", "Soil moisture content", "kg/(m^2)", UC_NONE},
   /* 4 */ {"VEG", "Vegetation", "%", UC_NONE},
   /* 5 */ {"WATR", "Water runoff", "kg/(m^2)", UC_NONE},
   /* 6 */ {"", "Evapotranspiration", "1/(kg^2 s)", UC_NONE},
   /* 7 */ {"", "Model terrain height", "m", UC_NONE},
   /* 8 */ {"", "Land use", "(1 Urban land, 2 agriculture, 3 Range Land, "
            "4 Deciduous forest, 5 Coniferous forest, 6 Forest/wetland, "
            "7 Water, 8 Wetlands, 9 Desert, 10 Tundra, 11 Ice, "
            "12 Tropical forest, 13 Savannah)", UC_NONE},
};

/* GRIB2 Code table 4.2 : 2.3 */
/* NCEP changed 0 to be "Soil type (as in Zobler)" I ignored them */
GRIB2ParmTable LandSoil[] = {
   /* 0 */ {"SOTYP", "Soil type", "(1 Sand, 2 Loamy sand, 3 Sandy loam, "
            "4 Silt loam, 5 Organic (redefined), 6 Sandy clay loam, "
            "7 Silt clay loam, 8 Clay loam, 9 Sandy clay, 10 Silty clay, "
            "11 Clay)", UC_NONE},
   /* 1 */ {"", "Upper layer soil temperature", "K", UC_NONE},
   /* 2 */ {"", "Upper layer soil moisture", "kg/(m^3)", UC_NONE},
   /* 3 */ {"", "Lower layer soil moisture", "kg/(m^3)", UC_NONE},
   /* 4 */ {"", "Bottom layer soil temperature", "K", UC_NONE},
};

/* GRIB2 Code table 4.2 : 3.0 */
GRIB2ParmTable SpaceImage[] = {
   /* 0 */ {"", "Scaled radiance", "numeric", UC_NONE},
   /* 1 */ {"", "Scaled albedo", "numeric", UC_NONE},
   /* 2 */ {"", "Scaled brightness temperature", "numeric", UC_NONE},
   /* 3 */ {"", "Scaled precipitable water", "numeric", UC_NONE},
   /* 4 */ {"", "Scaled lifted index", "numeric", UC_NONE},
   /* 5 */ {"", "Scaled cloud top pressure", "numeric", UC_NONE},
   /* 6 */ {"", "Scaled skin temperature", "numeric", UC_NONE},
   /* 7 */ {"", "Cloud mask", "(0 clear over water, 1 clear over land, "
            "2 cloud)", UC_NONE},
};

/* GRIB2 Code table 4.2 : 3.1 */
GRIB2ParmTable SpaceQuantitative[] = {
   /* 0 */ {"", "Estimated precipitation", "kg/(m^2)", UC_NONE},
};

/* GRIB2 Code table 4.2 : 10.0 */
GRIB2ParmTable OceanWaves[] = {
   /* 0 */ {"WVSP1", "Wave spectra (1)", "-", UC_NONE},
   /* 1 */ {"WVSP2", "Wave spectra (2)", "-", UC_NONE},
   /* 2 */ {"WVSP3", "Wave spectra (3)", "-", UC_NONE},
   /* 3 */ {"HTSGW", "Significant height of combined wind waves and swell", "m", UC_NONE},
   /* 4 */ {"WVDIR", "Direction of wind waves", "Degree true", UC_NONE},
   /* 5 */ {"WVHGT", "Significant height of wind waves", "m", UC_M2Feet}, /* NDFD override needed WaveHeight */
   /* 6 */ {"WVPER", "Mean period of wind waves", "s", UC_NONE},
   /* 7 */ {"SWDIR", "Direction of swell waves", "Degree true", UC_NONE},
   /* 8 */ {"SWELL", "Significant height of swell waves", "m", UC_NONE},
   /* 9 */ {"SWPER", "Mean period of swell waves", "s", UC_NONE},
   /* 10 */ {"DIRPW", "Primary wave direction", "Degree true", UC_NONE},
   /* 11 */ {"PERPW", "Primary wave mean period", "s", UC_NONE},
   /* 12 */ {"DIRSW", "Secondary wave direction", "Degree true", UC_NONE},
   /* 13 */ {"PERSW", "Secondary wave mean period", "s", UC_NONE},
};

/* GRIB2 Code table 4.2 : 10.1 */
GRIB2ParmTable OceanCurrents[] = {
   /* 0 */ {"DIRC", "Current direction", "Degree true", UC_NONE},
   /* 1 */ {"SPC", "Current speed", "m/s", UC_NONE},
   /* 2 */ {"UOGRD", "u-component of current", "m/s", UC_NONE},
   /* 3 */ {"VOGRD", "v-component of current", "m/s", UC_NONE},
};

/* GRIB2 Code table 4.2 : 10.2 */
GRIB2ParmTable OceanIce[] = {
   /* 0 */ {"ICEC", "Ice cover", "Proportion", UC_NONE},
   /* 1 */ {"ICETK", "Ice thinkness", "m", UC_NONE},
   /* 2 */ {"DICED", "Direction of ice drift", "Degree true", UC_NONE},
   /* 3 */ {"SICED", "Speed of ice drift", "m/s", UC_NONE},
   /* 4 */ {"UICE", "u-component of ice drift", "m/s", UC_NONE},
   /* 5 */ {"VICE", "v-component of ice drift", "m/s", UC_NONE},
   /* 6 */ {"ICEG", "Ice growth rate", "m/s", UC_NONE},
   /* 7 */ {"ICED", "Ice divergence", "1/s", UC_NONE},
};

/* GRIB2 Code table 4.2 : 10.3 */
GRIB2ParmTable OceanSurface[] = {
   /* 0 */ {"WTMP", "Water temperature", "K", UC_NONE},
   /* 1 */ {"DSLM", "Deviation of sea level from mean", "m", UC_NONE},
};

/* GRIB2 Code table 4.2 : 10.4 */
GRIB2ParmTable OceanSubSurface[] = {
   /* 0 */ {"MTHD", "Main thermocline depth", "m", UC_NONE},
   /* 1 */ {"MTHA", "Main thermocline anomaly", "m", UC_NONE},
   /* 2 */ {"TTHDP", "Transient thermocline depth", "m", UC_NONE},
   /* 3 */ {"SALTY", "Salinity", "kg/kg", UC_NONE},
};

NDFD_AbrevOverideTable NDFD_Overide[] = {
   /* 0 */ {"TMP", "T"},
   /* 1 */ {"TMAX", "MaxT"},
   /* 2 */ {"TMIN", "MinT"},
   /* 3 */ {"DPT", "Td"},
   /* 4 */ {"APCP", "QPF"},
   /* 5 */ {"SNOD", "SnowDepth"},
   /* 6 */ {"WDIR", "WindDir"},
   /* 7 */ {"WIND", "WindSpd"},
   /* 8 */ {"TCDC", "Sky"},
   /* 9 */ {"WVHGT", "WaveHeight"},
};
/* *INDENT-ON* */

/*****************************************************************************
 * Choose_GRIB2ParmTable() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Chooses the correct Parameter table depending on what is in the GRIB2
 * message's "Product Definition Section".
 *
 * ARGUMENTS
 * prodType = The product type (meteo, hydro, land, space, ocean, etc) (In)
 *      cat = The category inside the product (Input)
 * tableLen = The length of the returned table (Output)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: ParmTable (appropriate parameter table.)
 *
 * HISTORY
 *   1/2004 Arthur Taylor (MDL/RSIS): Created
 *
 * NOTES
 *****************************************************************************
 */
static GRIB2ParmTable *Choose_GRIB2ParmTable (int prodType, int cat,
                                              int *tableLen)
{
   enum { METEO_TEMP = 0, METEO_MOIST = 1, METEO_MOMENT = 2, METEO_MASS = 3,
      METEO_SW_RAD = 4, METEO_LW_RAD = 5, METEO_CLOUD = 6,
      METEO_THERMO_INDEX = 7, METEO_KINEMATIC_INDEX = 8, METEO_TEMP_PROB = 9,
      METEO_MOISTURE_PROB = 10, METEO_MOMENT_PROB = 11, METEO_MASS_PROB = 12,
      METEO_AEROSOL = 13, METEO_GAS = 14, METEO_RADAR = 15,
      METEO_RADAR_IMAGERY = 16, METEO_ELECTRO = 17, METEO_NUCLEAR = 18,
      METEO_ATMOS = 19, METEO_CCITT = 190, METEO_CCITT2 = 253
   };
   enum { HYDRO_BASIC = 0, HYDRO_PROB = 1 };
   enum { LAND_VEG = 0, LAND_SOIL = 3 };
   enum { SPACE_IMAGE = 0, SPACE_QUANTIT = 1 };
   enum { OCEAN_WAVES = 0, OCEAN_CURRENTS = 1, OCEAN_ICE = 2, OCEAN_SURF = 3,
      OCEAN_SUBSURF = 4
   };

   switch (prodType) {
      case 0:          /* Meteo type. */
         switch (cat) {
            case METEO_TEMP:
               *tableLen = sizeof (MeteoTemp) / sizeof (GRIB2ParmTable);
               return &MeteoTemp[0];
            case METEO_MOIST:
               *tableLen = sizeof (MeteoMoist) / sizeof (GRIB2ParmTable);
               return &MeteoMoist[0];
            case METEO_MOMENT:
               *tableLen = sizeof (MeteoMoment) / sizeof (GRIB2ParmTable);
               return &MeteoMoment[0];
            case METEO_MASS:
               *tableLen = sizeof (MeteoMass) / sizeof (GRIB2ParmTable);
               return &MeteoMass[0];
            case METEO_SW_RAD:
               *tableLen = (sizeof (MeteoShortRadiate) /
                            sizeof (GRIB2ParmTable));
               return &MeteoShortRadiate[0];
            case METEO_LW_RAD:
               *tableLen = (sizeof (MeteoLongRadiate) /
                            sizeof (GRIB2ParmTable));
               return &MeteoLongRadiate[0];
            case METEO_CLOUD:
               *tableLen = sizeof (MeteoCloud) / sizeof (GRIB2ParmTable);
               return &MeteoCloud[0];
            case METEO_THERMO_INDEX:
               *tableLen = sizeof (MeteoStability) / sizeof (GRIB2ParmTable);
               return &MeteoStability[0];
            case METEO_KINEMATIC_INDEX:
            case METEO_TEMP_PROB:
            case METEO_MOISTURE_PROB:
            case METEO_MOMENT_PROB:
            case METEO_MASS_PROB:
               *tableLen = 0;
               return NULL;
            case METEO_AEROSOL:
               *tableLen = sizeof (MeteoAerosols) / sizeof (GRIB2ParmTable);
               return &MeteoAerosols[0];
            case METEO_GAS:
               *tableLen = sizeof (MeteoGases) / sizeof (GRIB2ParmTable);
               return &MeteoGases[0];
            case METEO_RADAR:
               *tableLen = sizeof (MeteoRadar) / sizeof (GRIB2ParmTable);
               return &MeteoRadar[0];
            case METEO_RADAR_IMAGERY:
            case METEO_ELECTRO:
               *tableLen = 0;
               return NULL;
            case METEO_NUCLEAR:
               *tableLen = sizeof (MeteoNuclear) / sizeof (GRIB2ParmTable);
               return &MeteoNuclear[0];
            case METEO_ATMOS:
               *tableLen = sizeof (MeteoAtmos) / sizeof (GRIB2ParmTable);
               return &MeteoAtmos[0];
            case METEO_CCITT:
            case METEO_CCITT2:
               *tableLen = sizeof (MeteoText) / sizeof (GRIB2ParmTable);
               return &MeteoText[0];
            default:
               *tableLen = 0;
               return NULL;
         }
      case 1:          /* Hydro type. */
         switch (cat) {
            case HYDRO_BASIC:
               *tableLen = sizeof (HydroBasic) / sizeof (GRIB2ParmTable);
               return &HydroBasic[0];
            case HYDRO_PROB:
               *tableLen = sizeof (HydroProb) / sizeof (GRIB2ParmTable);
               return &HydroProb[0];
            default:
               *tableLen = 0;
               return NULL;
         }
      case 2:          /* Land type. */
         switch (cat) {
            case LAND_VEG:
               *tableLen = sizeof (LandVeg) / sizeof (GRIB2ParmTable);
               return &LandVeg[0];
            case LAND_SOIL:
               *tableLen = sizeof (LandSoil) / sizeof (GRIB2ParmTable);
               return &LandSoil[0];
            default:
               *tableLen = 0;
               return NULL;
         }
      case 3:          /* Space type. */
         switch (cat) {
            case SPACE_IMAGE:
               *tableLen = sizeof (SpaceImage) / sizeof (GRIB2ParmTable);
               return &SpaceImage[0];
            case SPACE_QUANTIT:
               *tableLen = (sizeof (SpaceQuantitative) /
                            sizeof (GRIB2ParmTable));
               return &SpaceQuantitative[0];
            default:
               *tableLen = 0;
               return NULL;
         }
      case 10:         /* ocean type. */
         switch (cat) {
            case OCEAN_WAVES:
               *tableLen = sizeof (OceanWaves) / sizeof (GRIB2ParmTable);
               return &OceanWaves[0];
            case OCEAN_CURRENTS:
               *tableLen = sizeof (OceanCurrents) / sizeof (GRIB2ParmTable);
               return &OceanCurrents[0];
            case OCEAN_ICE:
               *tableLen = sizeof (OceanIce) / sizeof (GRIB2ParmTable);
               return &OceanIce[0];
            case OCEAN_SURF:
               *tableLen = sizeof (OceanSurface) / sizeof (GRIB2ParmTable);
               return &OceanSurface[0];
            default:
               *tableLen = 0;
               return NULL;
         }
      default:
         *tableLen = 0;
         return NULL;
   }
}

/* *INDENT-OFF* */
GRIB2LocalTable NDFD_LclTable[] = {
   /* 0 */ {0, 1, 192, "Wx", "Weather string", "-", UC_NONE},
   /* grandfather'ed in a NDFD choice for POP. */
   /* 1 */ {0, 10, 8, "PoP12", "Prob of 0.01 In. of Precip", "%", UC_NONE},
   /* 2 */ {0, 14, 192, "O3MR", "Ozone Mixing Ratio", "kg/kg", UC_NONE},
   /* 3 */ {0, 14, 193, "OZCON", "Ozone Concentration", "PPB", UC_NONE},
};

/*
http://www.nco.ncep.noaa.gov/pmb/docs/grib2/GRIB2_parmeter_conversion_table.html
Updated this table last on 5/13/2004 
*/
GRIB2LocalTable NCEP_LclTable[] = {
   /*  0 */ {0, 0, 192, "SNOHF", "Snow Phase Change Heat Flux", "W/(m^2)", UC_NONE},
   /*  1 */ {0, 1, 192, "CRAIN", "Categorical Rain", "(0 no, 1 yes)", UC_NONE},
   /*  2 */ {0, 1, 193, "CFRZR", "Categorical Freezing Rain", "(0 no, 1 yes)", UC_NONE},
   /*  3 */ {0, 1, 194, "CICEP", "Categorical Ice Pellets", "(0 no, 1 yes)", UC_NONE},
   /*  4 */ {0, 1, 195, "CSNOW", "Categorical Snow", "(0 no, 1 yes)", UC_NONE},
   /*  5 */ {0, 1, 196, "CPRAT", "Convective Precipitation Rate", "(kg s)/(m^2)", UC_NONE},
   /*  6 */ {0, 1, 197, "MCONV", "Horizontal Moisture Divergence", "(kg s)/kg", UC_NONE},
   /*  7 */ {0, 1, 198, "", "Percent Frozen Precipitation", "-", UC_NONE},
   /*  8 */ {0, 1, 199, "PEVAP", "Potential Evaporation", "kg/(m^2)", UC_NONE},
   /*  9 */ {0, 1, 200, "PEVPR", "Potential Evaporation Rate", "W/(m^2)", UC_NONE},
   /* 10 */ {0, 1, 201, "SNOWC", "Snow Cover", "%", UC_NONE},
   /* 11 */ {0, 1, 202, "FRAIN", "Rain Fraction of Total Liquid Water", "", UC_NONE},
   /* 12 */ {0, 1, 203, "FRIME", "Rime Factor", "-", UC_NONE},
   /* 13 */ {0, 1, 204, "TCOLR", "Total Column Integrated Rain", "kg/(m/m)", UC_NONE},
   /* 14 */ {0, 1, 205, "TCOLS", "Total Column Integrated Snow", "kg/(m/m)", UC_NONE},
   /* 15 */ {0, 2, 192, "VWSH", "Vertical speed shear", "1/s", UC_NONE},
   /* 16 */ {0, 2, 193, "MFLX", "Horizontal Momentum Flux", "N/(m^2)", UC_NONE},
   /* 17 */ {0, 2, 194, "USTM", "U-Component Storm Motion", "m/s", UC_NONE},
   /* 18 */ {0, 2, 195, "VSTM", "V-Component Storm Motion", "m/s", UC_NONE},
   /* 19 */ {0, 2, 196, "CD", "Drag Coefficient", "-", UC_NONE},
   /* 20 */ {0, 2, 197, "FRICV", "Frictional Velocity", "m/s", UC_NONE},
   /* 21 */ {0, 3, 192, "MSLET", "Mean Sea Level Pressure (Eta Reduction)", "Pa", UC_NONE},
   /* 22 */ {0, 3, 193, "5WAVH", "5-Wave Geopotential Height", "gpm", UC_NONE},
   /* 23 */ {0, 3, 194, "U-GWD", "Zonal Flux of Gravity Wave Stress", "N/(m^2)", UC_NONE},
   /* 24 */ {0, 3, 195, "V-GWD", "Meridional Flux of Gravity Wave Stress", "N/(m^2)", UC_NONE},
   /* 25 */ {0, 3, 196, "HPBL", "Planetary Boundary Layer Height", "m", UC_NONE},
   /* 26 */ {0, 3, 197, "5WAVA", "5-Wave Geopotential Height Anomaly", "gpm", UC_NONE},
   /* 27 */ {0, 4, 192, "DSWRF", "Downward Short-Wave Rad. Flux", "W/(m^2)", UC_NONE},
   /* 28 */ {0, 4, 193, "USWRF", "Upward Short-Wave Rad. Flux", "W/(m^2)", UC_NONE},
   /* 29 */ {0, 5, 192, "DLWRF", "Downward Long-Wave Rad. Flux", "W/(m^2)", UC_NONE},
   /* 30 */ {0, 5, 193, "ULWRF", "Upward Long-Wave Rad. Flux", "W/(m^2)", UC_NONE},
   /* 31 */ {0, 6, 192, "CDLYR", "Non-Convective Cloud Cover", "%", UC_NONE},
   /* 32 */ {0, 6, 193, "CWORK", "Cloud Work Function", "J/kg", UC_NONE},
   /* 33 */ {0, 6, 194, "CUEFI", "Convective Cloud Efficiency", "-", UC_NONE},
   /* 34 */ {0, 6, 195, "TCOND", "Total Condensate", "kg/kg", UC_NONE},
   /* 35 */ {0, 6, 196, "TCOLW", "Total Column-Integrated Cloud Water", "kg/(m/m)", UC_NONE},
   /* 36 */ {0, 6, 197, "TCOLI", "Total Column-Integrated Cloud Ice", "kg/(m/m)", UC_NONE},
   /* 37 */ {0, 6, 198, "TCOLC", "Total Column-Integrated Condensate", "kg/(m/m)", UC_NONE},
   /* 38 */ {0, 6, 199, "FICE", "Ice fraction of total condensate", "-", UC_NONE},
   /* 39 */ {0, 7, 192, "LFTX", "Surface Lifted Index", "K", UC_NONE},
   /* 40 */ {0, 7, 193, "4LFTX", "Best (4 layer) Lifted Index", "K", UC_NONE},
   /* 41 */ {0, 7, 194, "RI", "Richardson Number", "-", UC_NONE},
   /* 42 */ {0, 14, 192, "O3MR", "Ozone Mixing Ratio", "kg/kg", UC_NONE},
   /* 43 */ {0, 14, 193, "OZCON", "Ozone Concentration", "PPB", UC_NONE},
   /* 44 */ {0, 14, 194, "OZCAT", "Categorical Ozone Concentration", "unknown", UC_NONE},
   /* 45 */ {0, 19, 192, "MXSALB", "Maximum Snow Albedo", "%", UC_NONE},
   /* 46 */ {0, 19, 193, "SNFALB", "Snow-Free Albedo", "%", UC_NONE},
   /* 47 */ {0, 191, 192, "NLAT", "Latitude (-90 to 90)", "deg", UC_NONE},
   /* 48 */ {0, 191, 193, "ELON", "East Longitude (0 to 360)", "deg", UC_NONE},
   /* 49 */ {0, 191, 194, "secPriorInitRef", "Seconds prior to initial reference time", "sec", UC_NONE},
   /* 50 */ {1, 0, 192, "BGRUN", "Baseflow-Groundwater Runoff", "kg/(m^2)", UC_NONE},
   /* 51 */ {1, 0, 193, "SSRUN", "Storm Surface Runoff", "kg/(m^2)", UC_NONE},
   /* 52 */ {2, 0, 192, "SOILW", "Volumetric Soil Moisture Content", "fraction", UC_NONE},
   /* 53 */ {2, 0, 193, "GFLUX", "Ground Heat Flux", "W/(m^2)", UC_NONE},
   /* 54 */ {2, 0, 194, "MSTAV", "Moisture Availability", "%", UC_NONE},
   /* 55 */ {2, 0, 195, "SFEXC", "Exchange Coefficient", "(kg/(m^3))(m/s)", UC_NONE},
   /* 56 */ {2, 0, 196, "CNWAT", "Plant Canopy Surface Water", "kg/(m^2)", UC_NONE},
   /* 57 */ {2, 0, 197, "BMIXL", "Blackadar's Mixing Length Scale", "m", UC_NONE},
   /* 58 */ {2, 0, 198, "VGTYP", "Vegetation Type", "0..13", UC_NONE},
   /* 59 */ {2, 0, 199, "CCOND", "Canopy Conductance", "m/s", UC_NONE},
   /* 60 */ {2, 0, 200, "RSMIN", "Minimal Stomatal Resistance", "s/m", UC_NONE},
   /* 61 */ {2, 0, 201, "WILT", "Wilting Point", "fraction", UC_NONE},
   /* 62 */ {2, 0, 202, "RCS", "Solar parameter in canopy conductance", "fraction", UC_NONE},
   /* 63 */ {2, 0, 203, "RCT", "Temperature parameter in canopy conductance", "fraction", UC_NONE},
   /* 64 */ {2, 0, 204, "RCQ", "Humidity parameter in canopy conductance", "fraction", UC_NONE},
   /* 65 */ {2, 0, 205, "RCSOL", "Soil moisture parameter in canopy conductance", "fraction", UC_NONE},
   /* 66 */ {2, 3, 192, "SOILL", "Liquid Volumetric Soil Moisture", "fraction", UC_NONE},
   /* 67 */ {2, 3, 193, "RLYRS", "Number of Soil Layers in Root Zone", "-", UC_NONE},
   /* 68 */ {2, 3, 194, "SLTYP", "Surface Slope Type", "Index", UC_NONE},
   /* 69 */ {2, 3, 195, "SMREF", "Transpiration Stress-onset (soil moisture)", "fraction", UC_NONE},
   /* 70 */ {2, 3, 196, "SMDRY", "Direct Evaporation Cease (soil moisture)", "fraction", UC_NONE},
   /* 71 */ {2, 3, 197, "POROS", "Soil Porosity", "fraction", UC_NONE},
   /* 72 */ {3, 1, 192, "ScatEstUWind", "Scatterometer Estimated U Wind", "unknown", UC_NONE},
   /* 73 */ {3, 1, 193, "ScatEstVWind", "Scatterometer Estimated V Wind", "unknown", UC_NONE},
};
/* *INDENT-ON* */

int IsData_NDFD (unsigned short int center, unsigned short int subcenter)
{
   return ((center == 8) &&
           ((subcenter == GRIB2MISSING_2) || (subcenter == 0)));
}

/*****************************************************************************
 * Choose_LocalParmTable() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Chooses the local parameter table for a given center/subcenter.
 * Typically this is called after the default Choose_ParmTable was tried,
 * since it consists of all the local specs, and one has to linearly walk
 * through the table.
 *
 * ARGUMENTS
 *    center = The center that created the data. (Input)
 * subcenter = The subcenter that created the data. (Input)
 *  tableLen = The length of the returned table (Output)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: LocalParmTable (appropriate parameter table.)
 *
 * HISTORY
 *   1/2004 Arthur Taylor (MDL/RSIS): Created
 *
 * NOTES
 *****************************************************************************
 */
static GRIB2LocalTable *Choose_LocalParmTable (unsigned short int center,
                                               unsigned short int subcenter,
                                               int *tableLen)
{
   switch (center) {
      case 7:          /* NWS NCEP */
         switch (subcenter) {
            case 0:    /* GRIB2 Table 1? */
               *tableLen = sizeof (NCEP_LclTable) / sizeof (GRIB2LocalTable);
               return &NCEP_LclTable[0];
         }
      case 8:          /* NWS Telecomunications gateway */
         switch (subcenter) {
            case GRIB2MISSING_2: /* NDFD */
            case 0:    /* NDFD */
               *tableLen = sizeof (NDFD_LclTable) / sizeof (GRIB2LocalTable);
               return &NDFD_LclTable[0];
            default:
               *tableLen = 0;
               return NULL;
         }
      default:
         *tableLen = 0;
         return NULL;
   }
}

/*****************************************************************************
 * ParseElemName() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Converts a prodType, template, category and subcategory quadruple to the
 * ASCII string abreviation of that variable.
 *   For example: 0, 0, 0, 0, = "T" for temperature.
 *
 * ARGUMENTS
 *    center = The center that created the data. (Input)
 * subcenter = The subcenter that created the data. (Input)
 *  prodType = The GRIB2, section 0 product type. (Input)
 *   templat = The GRIB2 section 4 template number. (Input)
 *       cat = The GRIB2 section 4 "General category of Product." (Input)
 *    subcat = The GRIB2 section 4 "Specific subcategory of Product". (Input)
 *   lenTime = The length of time over which statistics are done
 *             (see template 4.8). (Input)
 *      name = Short name for the data set (T, MaxT, etc) (Output)
 *   comment = Long form of the name (Temperature, etc) (Output)
 *      unit = What unit this variable is originally in (Output)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: void
 *
 * HISTORY
 *   1/2004 Arthur Taylor (MDL/RSIS): Re-Created.
 *   6/2004 AAT: Added deltTime (because of Ozone issues).
 *
 * NOTES
 *****************************************************************************
 */
void ParseElemName (unsigned short int center, unsigned short int subcenter,
                    int prodType, int templat, int cat, int subcat,
                    long int lenTime, char **name, char **comment,
                    char **unit, int *convert)
{
   GRIB2ParmTable *table;
   GRIB2LocalTable *local;
   int tableLen;
   int i;

   /* Check for over-ride cases. */
   /* These were originally just NDFD over-rides for ozone, but I think they
    * will be useful for ozone data that originated elsewhere. */
   if ((templat == 8) && (prodType == 0) && (cat == 14) && (subcat == 193)) {
      if (lenTime == 8) {
         *name = "Ozone08";
         *comment = "8 hour Average Ozone Concentration";
         *unit = "PPB";
         *convert = UC_NONE;
         return;
      } else if (lenTime == 1) {
         *name = "Ozone01";
         *comment = "1 hour Average Ozone Concentration";
         *unit = "PPB";
         *convert = UC_NONE;
         return;
      } else {
         *name = "AVGOZCON";
         *comment = "Average Ozone Concentration";
         *unit = "PPB";
         *convert = UC_NONE;
         return;
      }
   }
   /* Current NDFD choice for POP. */
   /* template 9 Precip_Tot -> PoP12 */
   if (IsData_NDFD (center, subcenter)) {
      if ((templat == 9) && (prodType == 0) && (cat == 1) && (subcat == 8)) {
         *name = "PoP12";
         *comment = "Prob of 0.01 In. of Precip";
         *unit = "%";
         *convert = UC_NONE;
         return;
      }
   }
   /* Generic tables. */
   table = Choose_GRIB2ParmTable (prodType, cat, &tableLen);
   if (table != NULL) {
      if ((subcat >= 0) && (subcat < tableLen)) {
         *name = table[subcat].name;
         *comment = table[subcat].comment;
         *unit = table[subcat].unit;
         *convert = table[subcat].convert;
         if (IsData_NDFD (center, subcenter)) {
            for (i = 0; i < (sizeof (NDFD_Overide) /
                             sizeof (NDFD_AbrevOverideTable)); i++) {
               if (strcmp (NDFD_Overide[i].GRIB2name, *name) == 0) {
                  *name = NDFD_Overide[i].NDFDname;
                  break;
               }
            }
         }
         return;
      }
   }
   /* Local use tables. */
   local = Choose_LocalParmTable (center, subcenter, &tableLen);
   if (local != NULL) {
      for (i = 0; i < tableLen; i++) {
         if ((prodType == local[i].prodType) && (cat == local[i].cat) &&
             (subcat == local[i].subcat)) {
            *name = local[i].name;
            *comment = local[i].comment;
            *unit = local[i].unit;
            *convert = local[i].convert;
            return;
         }
      }
   }
   *name = "";
   *comment = "unknown";
   *unit = "-";
   *convert = UC_NONE;
   return;
}

/*****************************************************************************
 * ParseElemName2() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Converts a prodType, template, category and subcategory quadruple to the
 * ASCII string abreviation of that variable.
 *   For example: 0, 0, 0, 0, = "T" for temperature.
 *
 * ARGUMENTS
 * prodType = The GRIB2, section 0 product type. (Input)
 *  templat = The GRIB2 section 4 template number. (Input)
 *      cat = The GRIB2 section 4 "General category of Product." (Input)
 *   subcat = The GRIB2 section 4 "Specific subcategory of Product". (Input)
 *     name = Where to store the result (assumed already allocated to at
 *            least 15 bytes) (Output)
 *  comment = Extra info about variable (assumed already allocated to at
 *            least 100 bytes) (Output)
 *     unit = What unit this variable is in. (assumed already allocated to at
 *            least 20 bytes) (Output)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: char *
 *   Same as 'strcpy', ie it returns name.
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *  11/2002 AAT: Added MOIST_TOT_SNOW (and switched MOIST_SNOWAMT to
 *               SnowDepth)
 *  12/2002 (TK,AC,TB,&MS): Code Review.
 *   2/2003 AAT: moved from degrib.c to metaparse.c
 *              (Reason: primarily for Sect2 Parsing)
 *              (renamed from ElementName to ParseElemName)
 *   4/2003 AAT: Added the comment as a return element.(see GRIB2 discipline)
 *   6/2003 AAT: Added the unit as a return element.
 *   6/2003 AAT: Added Wave Height.
 *
 * NOTES
 *   Similar to GRIB1_Table2LookUp... May want to take this and the unit
 * stuff and combine them into a module.
 *****************************************************************************
 */
/*
static void ParseElemName2 (int prodType, int templat, int cat, int subcat,
                            char *name, char *comment, char *unit)
{
   if (prodType == 0) {
      if (cat == CAT_TEMP) { * 0 *
         switch (subcat) {
            case TEMP_TEMP: * 0 *
               strcpy (comment, "Temperature [K]");
               strcpy (name, "T");
               strcpy (unit, "[K]");
               return;
            case TEMP_MAXT: * 4 *
               strcpy (comment, "Maximum temperature [K]");
               strcpy (name, "MaxT");
               strcpy (unit, "[K]");
               return;
            case TEMP_MINT: * 5 *
               strcpy (comment, "Minimum temperature [K]");
               strcpy (name, "MinT");
               strcpy (unit, "[K]");
               return;
            case TEMP_DEW_TEMP: * 6 *
               strcpy (comment, "Dew point temperature [K]");
               strcpy (name, "Td");
               strcpy (unit, "[K]");
               return;
            case TEMP_WINDCHILL: * 13 *
               strcpy (comment, "Wind chill factor [K]");
               strcpy (name, "WCI");
               strcpy (unit, "[K]");
               return;
            case TEMP_HEAT: * 12 *
               strcpy (comment, "Heat index [K]");
               strcpy (name, "HeatIndex");
               strcpy (unit, "[K]");
               return;
         }
      } else if (cat == CAT_MOIST) { * 1 *
         switch (subcat) {
            case MOIST_REL_HUMID: * 1 *
               strcpy (comment, "Relative Humidity [%]");
               strcpy (name, "RH");
               strcpy (unit, "[%]");
               return;
            case MOIST_PRECIP_TOT: * 8 *
               if (templat == 9) { * template number 9 implies prob. *
                  strcpy (comment, "Prob of 0.01 In. of Precip [%]");
                  strcpy (name, "PoP12");
                  strcpy (unit, "[%]");
                  return;
               } else {
                  strcpy (comment, "Total precipitation [kg/(m^2)]");
                  strcpy (name, "QPF");
                  strcpy (unit, "[kg/(m^2)]");
                  return;
               }
            case MOIST_SNOWAMT: * 11 *
               strcpy (comment, "Snow Depth [m]");
               strcpy (name, "SnowDepth");
               strcpy (unit, "[m]");
               return;
            case MOIST_TOT_SNOW: * 29 *
               strcpy (comment, "Total snowfall [m]");
               strcpy (name, "SnowAmt");
               strcpy (unit, "[m]");
               return;
            case 192:  * local use moisture. *
               strcpy (comment, "Weather (local use moisture) [-]");
               strcpy (name, "Wx");
               strcpy (unit, "[-]");
               return;
         }
      } else if (cat == CAT_MOMENT) { * 2 *
         switch (subcat) {
            case MOMENT_WINDDIR: * 0 *
               strcpy (comment, "Wind direction (from which blowing) "
                       "[deg true]");
               strcpy (name, "WindDir");
               strcpy (unit, "[deg true]");
               return;
            case MOMENT_WINDSPD: * 1 *
               strcpy (comment, "Wind speed [m/s]");
               strcpy (name, "WindSpd");
               strcpy (unit, "[m/s]");
               return;
         }
      } else if (cat == CAT_CLOUD) { * 6 *
         switch (subcat) {
            case CLOUD_COVER: * 1 *
               strcpy (comment, "Total cloud cover [%]");
               strcpy (name, "Sky");
               strcpy (unit, "[%]");
               return;
         }
      } else if (cat == CAT_MOISTURE_PROB) { * 10 *
         if (subcat == 8) { * grandfather'ed in. *
            strcpy (comment, "Prob of 0.01 In. of Precip [%]");
            strcpy (name, "PoP12");
            strcpy (unit, "[%]");
            return;
         }
      }
   } else if (prodType == 10) {
      if (cat == OCEAN_CAT_WAVES) { * 0 *
         if (subcat == OCEAN_WAVE_SIG_HT_WV) { * 5 *
            strcpy (comment, "Significant height of wind waves [m]");
            strcpy (name, "WaveHeight");
            strcpy (unit, "[m]");
            return;
         }
      }
   }
   strcpy (name, "");
   strcpy (comment, "unknown");
   strcpy (unit, "[-]");
   return;
}
*/

/*****************************************************************************
 * ComputeUnit() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Sets m, and b for equation y = mx + b, where x is in the unit
 * specified by GRIB2, and y is the one specified by f_unit.  The default
 * is m = 1, b = 0.
 *
 * Currently:
 *   For f_unit = 1 (english) we return Fahrenheit, knots, and inches for
 * temperature, wind speed, and amount of snow or rain.  The original units
 * are Kelvin, m/s, kg/m**2.
 *   For f_unit = 2 (metric) we return Celsius instead of Kelvin.
 *
 * ARGUMENTS
 * convert = The enumerated type describing the type of conversion. (Input)
 *  f_unit = What type of unit to return (see above) (Input).
 *   unitM = M in equation y = m x + b (Output)
 *   unitB = B in equation y = m x + b (Output)
 *    name = Where to store the result (assumed already allocated to at
 *           least 15 bytes) (Output)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: int
 *   0 if we set M and B, 1 if we used defaults.
 *
 * HISTORY
 *   1/2004 Arthur Taylor (MDL/RSIS): Re-Created.
 *
 * NOTES
 *****************************************************************************
 */
int ComputeUnit (int convert, sChar f_unit, double *unitM, double *unitB,
                 char *name)
{
   switch (convert) {
      case UC_NONE:
         break;
      case UC_K2F:     /* Convert from Kelvin to F or C. */
         if (f_unit == 1) {
            strcpy (name, "[F]");
            *unitM = 9. / 5.;
            /* 32 - (9/5 * 273.15) = 32 - 491.67 = -459.67. */
            *unitB = -459.67;
            return 0;
         } else if (f_unit == 2) {
            strcpy (name, "[C]");
            *unitM = 1;
            *unitB = -273.15;
            return 0;
         }
         break;
      case UC_InchWater: /* Convert from kg/(m^2) to inches water. */
         if (f_unit == 1) {
            strcpy (name, "[inch]");
            /* 
             * kg/m**2 / density of water (1000 kg/m**3)
             * 1/1000 m * 1/2.54 in/cm * 100 cm/m = 1/25.4 inches
             */
            *unitM = 1. / 25.4;
            *unitB = 0;
            return 0;
         }
         break;
      case UC_M2Feet:  /* Convert from meters to feet. */
         if (f_unit == 1) {
            /* 1 (m) * (100cm/m) * (inch/2.54cm) * (ft/12inch) = X (ft) */
            strcpy (name, "[feet]");
            *unitM = 100. / 30.48;
            *unitB = 0;
            return 0;
         }
         break;
      case UC_M2Inch:  /* Convert from meters to inches. */
         if (f_unit == 1) {
            strcpy (name, "[inch]");
            *unitM = 100. / 2.54; /* inch / m */
            *unitB = 0;
            return 0;
         }
         break;
      case UC_MS2Knots: /* Convert from m/s to knots. */
         if (f_unit == 1) {
            strcpy (name, "[knots]");
            *unitM = 3600. / 1852.; /* knot / m s**-1 */
            *unitB = 0;
            return 0;
         }
         break;
   }
   /* Default case is for the unit in the GRIB2 document. */
   strcpy (name, "[GRIB2 unit]");
   *unitM = 1;
   *unitB = 0;
   return 1;
}

/*****************************************************************************
 * ComputeUnit2() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Sets m, and b for equation y = mx + b, where x is in the unit
 * specified by GRIB2, and y is the one specified by f_unit.  The default
 * is m = 1, b = 0.
 *
 * Currently:
 *   For f_unit = 1 (english) we return Fahrenheit, knots, and inches for
 * temperature, wind speed, and amount of snow or rain.  The original units
 * are Kelvin, m/s, kg/m**2.
 *   For f_unit = 2 (metric) we return Celsius instead of Kelvin.
 *
 * ARGUMENTS
 * prodType = The GRIB2, section 0 product type. (Input)
 *  templat = The GRIB2 section 4 template number. (Input)
 *      cat = The GRIB2 section 4 "General category of Product." (Input)
 *   subcat = The GRIB2 section 4 "Specific subcategory of Product". (Input)
 *   f_unit = What type of unit to return (see above) (Input).
 *    unitM = M in equation y = m x + b (Output)
 *    unitB = B in equation y = m x + b (Output)
 *     name = Where to store the result (assumed already allocated to at
 *            least 15 bytes) (Output)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: int
 *   0 if we set M and B, 1 if we used defaults.
 *
 * HISTORY
 *  11/2002 Arthur Taylor (MDL/RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
/*
static int ComputeUnit2 (int prodType, int templat, int cat, int subcat,
                         sChar f_unit, double *unitM, double *unitB,
                         char *name)
{
   if (prodType == 0) {
      switch (cat) {
         case CAT_TEMP:
            * subcat 8 is K/m, 10, 11 is W/m**2 *
            if ((subcat < 16) && (subcat != 8) &&
                (subcat != 10) && (subcat != 11)) {
               if (f_unit == 1) {
                  strcpy (name, "[F]");
                  *unitM = 9. / 5.;
                  * 32 - (9/5 * 273.15) = 32 - 491.67 = -459.67. *
                  *unitB = -459.67;
                  return 0;
               } else if (f_unit == 2) {
                  strcpy (name, "[C]");
                  *unitM = 1;
                  *unitB = -273.15;
                  return 0;
               }
            }
            break;
         case CAT_MOIST:
            if (subcat == MOIST_PRECIP_TOT) {
               if (templat != 9) { * template number != 9 implies QPF. *
                  if (f_unit == 1) {
                     strcpy (name, "[inch]");
                     *
                      * kg/m**2 / density of water (1000 kg/m**3)
                      * 1/1000 m * 1/2.54 in/cm * 100 cm/m = 1/25.4 inches
                      *
                     *unitM = 1. / 25.4;
                     *unitB = 0;
                     return 0;
                  }
               }
            }
            if ((subcat == MOIST_SNOWAMT) || (subcat == MOIST_TOT_SNOW)) {
               if (f_unit == 1) {
                  strcpy (name, "[inch]");
                  *unitM = 100. / 2.54; * inch / m *
                  *unitB = 0;
                  return 0;
               }
            }
            break;
         case CAT_MOMENT:
            if (subcat == MOMENT_WINDSPD) {
               if (f_unit == 1) {
                  strcpy (name, "[knots]");
                  *unitM = 3600. / 1852.; * knot / m s**-1 *
                  *unitB = 0;
                  return 0;
               }
            }
            break;
      }
   } else if (prodType == 10) {
      if (cat == OCEAN_CAT_WAVES) { * 0 *
         if (subcat == OCEAN_WAVE_SIG_HT_WV) { * 5 *
            if (f_unit == 1) {
               * 1 (m) * (100cm/m) * (inch/2.54cm) * (ft/12inch) = X (ft) *
               strcpy (name, "[feet]");
               *unitM = 100. / 30.48;
               *unitB = 0;
               return 0;
            }
         }
      }
   }
   * Default case is for the unit in the GRIB2 document. *
   strcpy (name, "[GRIB2 unit]");
   *unitM = 1;
   *unitB = 0;
   return 1;
}
*/

/* GRIB2 Code Table 4.5 */
/* *INDENT-OFF* */
GRIB2SurfTable Surface[] = {
   /* 1 */ {"SFC", "Ground or water surface", "-"},
   /* 2 */ {"CBL", "Cloud base level", "-"},
   /* 3 */ {"CTL", "Level of cloud tops", "-"},
   /* 4 */ {"0DEG", "Level of 0 degree C isotherm", "-"},
   /* 5 */ {"ADCL", "Level of adiabatic condensation lifted from the surface", "-"},
   /* 6 */ {"MWSL", "Maximum wind level", "-"},
   /* 7 */ {"TRO", "Tropopause", "-"},
   /* 8 */ {"NTAT", "Nominal top of atmosphere", "-"},
   /* 9 */ {"SEAB", "Sea bottom", "-"},
   /* 20 */ {"TMPL", "Isothermal level", "K"},
   /* 100 */ {"ISBL", "Isobaric surface", "Pa"},
   /* 101 */ {"MSL", "Mean sea level", "-"},
   /* 102 */ {"GPML", "Specific altitude above mean sea level", "m"},
   /* 103 */ {"HTGL", "Specified height level above ground", "m"},
   /* 104 */ {"SIGL", "Sigma level", "'sigma' value"},
   /* 105 */ {"HYBL", "Hybrid level", "-"},
   /* 106 */ {"DBLL", "Depth below land surface", "m"},
   /* 107 */ {"THEL", "Isentropic (theta) level", "K"},
   /* 108 */ {"SPDL", "Level at specified pressure difference from ground to level", "Pa"},
   /* 109 */ {"PVL", "Potential vorticity surface", "(K M^2)/(kg s)"},
   /* 111 */ {"EtaL", "Eta* level", "-"},
   /* 117 */ {"unknown", "Mixed layer depth", "m"}, /* unknown abbrev */
   /* 160 */ {"DBSL", "Depth below sea level", "m"},
};
/* *INDENT-ON* */

/*****************************************************************************
 * Table45Index() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   To figure out the index into the table45 array (used for Code Table 4.5)
 *
 * ARGUMENTS
 * i = The original index to look up. (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: int
 * index of (i) in Table45 (191 denotes reserved)
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
int Table45Index (int i)
{
   int ans = i;

   ans -= 1;            /* 1 reserved value. (0) */
   if (i < 1)
      return 191;       /* Denotes 'Reserved' */
   if (i < 10)
      return ans;

   ans -= 10;           /* 10 reserved values. (10-19) */
   if (i < 20)
      return 191;
   if (i < 21)
      return ans;

   ans -= 79;           /* 79 reserved values. (21-99) */
   if (i < 100)
      return 191;
   if (i < 112)
      return ans;

   ans -= 5;            /* 5 reserved values. (112-116) */
   if (i < 117)
      return 191;
   if (i < 118)
      return ans;

   ans -= 42;           /* 42 reserved values. (118-159) */
   if (i < 160)
      return 191;
   if (i < 161)
      return ans;
   return i;
}

void ParseLevelName (unsigned short int center, unsigned short int subcenter,
                     uChar surfType, double value, sChar f_sndValue,
                     double sndValue, char **shortLevelName,
                     char **longLevelName)
{
   int index = Table45Index (surfType);

   /* Check if index is defined... 191 is undefined. */
   free (*shortLevelName);
   *shortLevelName = NULL;
   free (*longLevelName);
   *longLevelName = NULL;
   if (f_sndValue) {
      if (index >= 191) {
         reallocSprintf (shortLevelName, "%d-%d-%s(%d)", (int) (value + .5),
                         (int) (sndValue + .5), "reserved", surfType);
         reallocSprintf (longLevelName, "%d-%d[%s] %s(%d) (%s)",
                         (int) (value + .5), (int) (sndValue + .5), "-",
                         "reserved", surfType, "");
      } else if (index > 191) {
         reallocSprintf (shortLevelName, "%d-%d-%s(%d)", (int) (value + .5),
                         (int) (sndValue + .5), "reservedLocal", surfType);
         reallocSprintf (longLevelName, "%d[%s] %s(%d) (%s)",
                         (int) (value + .5), "-", "reservedLocal", surfType,
                         "");
      } else {
         reallocSprintf (shortLevelName, "%d-%d-%s", (int) (value + .5),
                         (int) (sndValue + .5), Surface[index].name);
         reallocSprintf (longLevelName, "%d-%d[%s] %s=\"%s\"",
                         (int) (value + .5), (int) (sndValue + .5),
                         Surface[index].unit, Surface[index].name,
                         Surface[index].comment);
      }
   } else {
      if (index == 191) {
         reallocSprintf (shortLevelName, "%d-%s(%d)", (int) (value + .5),
                         "reserved", surfType);
         reallocSprintf (longLevelName, "%d[%s] %s(%d) (%s)",
                         (int) (value + .5), "-", "reserved", surfType, "");
      } else if (index > 191) {
         reallocSprintf (shortLevelName, "%d-%s(%d)", (int) (value + .5),
                         "reservedLocal", surfType);
         reallocSprintf (longLevelName, "%d[%s] %s(%d) (%s)",
                         (int) (value + .5), "-", "reservedLocal", surfType,
                         "");
      } else {
         reallocSprintf (shortLevelName, "%d-%s", (int) (value + .5),
                         Surface[index].name);
         reallocSprintf (longLevelName, "%d[%s] %s=\"%s\"",
                         (int) (value + .5), Surface[index].unit,
                         Surface[index].name, Surface[index].comment);
      }
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/degrib/RCS/metaname.c,v $";
 static char rcs_id2[] = "$Id: metaname.c,v 1.1 2004/09/16 17:14:09 dsa Exp $";}
/*  ===================================================  */

}
