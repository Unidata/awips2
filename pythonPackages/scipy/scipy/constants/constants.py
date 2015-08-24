"""
Collection of physical constants and conversion factors.

Most constants are in SI units, so you can do
print '10 mile per minute is', 10*mile/minute, 'm/s or', 10*mile/(minute*knot), 'knots'

The list is not meant to be comprehensive, but just a convenient list for everyday use.
"""
<<<<<<< HEAD
=======
from __future__ import division, print_function, absolute_import
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

"""
BasSw 2006
physical constants: imported from CODATA
unit conversion: see e.g. NIST special publication 811
Use at own risk: double-check values before calculating your Mars orbit-insertion burn.
Some constants exist in a few variants, which are marked with suffixes.
The ones without any suffix should be the most common one.
"""

import math as _math
<<<<<<< HEAD
from codata import value as _cd

#mathematical constants
pi = _math.pi
golden = golden_ratio = (1 + _math.sqrt(5)) / 2

#SI prefixes
=======
from .codata import value as _cd
import numpy as _np

# mathematical constants
pi = _math.pi
golden = golden_ratio = (1 + _math.sqrt(5)) / 2

# SI prefixes
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
yotta = 1e24
zetta = 1e21
exa = 1e18
peta = 1e15
tera = 1e12
giga = 1e9
mega = 1e6
kilo = 1e3
hecto = 1e2
deka = 1e1
deci = 1e-1
centi = 1e-2
milli = 1e-3
micro = 1e-6
nano = 1e-9
pico = 1e-12
femto = 1e-15
atto = 1e-18
zepto = 1e-21

<<<<<<< HEAD
#binary prefixes
=======
# binary prefixes
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
kibi = 2**10
mebi = 2**20
gibi = 2**30
tebi = 2**40
pebi = 2**50
exbi = 2**60
zebi = 2**70
yobi = 2**80

<<<<<<< HEAD
#physical constants
=======
# physical constants
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
c = speed_of_light = _cd('speed of light in vacuum')
mu_0 = 4e-7*pi
epsilon_0 = 1 / (mu_0*c*c)
h = Planck = _cd('Planck constant')
hbar = h / (2 * pi)
G = gravitational_constant = _cd('Newtonian constant of gravitation')
g = _cd('standard acceleration of gravity')
e = elementary_charge = _cd('elementary charge')
R = gas_constant = _cd('molar gas constant')
alpha = fine_structure = _cd('fine-structure constant')
N_A = Avogadro = _cd('Avogadro constant')
<<<<<<< HEAD
k = Bolzmann = _cd('Boltzmann constant')
sigma = Stefan_Bolzmann = _cd('Stefan-Boltzmann constant')
Wien = _cd('Wien displacement law constant')
Rydberg = _cd('Rydberg constant')

#weight in kg
gram = 1e-3
metric_ton = 1e3
grain = 64.79891e-6
lb = pound = 7000 * grain #avoirdupois
=======
k = Boltzmann = _cd('Boltzmann constant')
sigma = Stefan_Boltzmann = _cd('Stefan-Boltzmann constant')
Wien = _cd('Wien wavelength displacement law constant')
Rydberg = _cd('Rydberg constant')

# weight in kg
gram = 1e-3
metric_ton = 1e3
grain = 64.79891e-6
lb = pound = 7000 * grain  # avoirdupois
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
oz = ounce = pound / 16
stone = 14 * pound
long_ton = 2240 * pound
short_ton = 2000 * pound

<<<<<<< HEAD
troy_ounce = 480 * grain #only for metals / gems
=======
troy_ounce = 480 * grain  # only for metals / gems
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
troy_pound = 12 * troy_ounce
carat = 200e-6

m_e = electron_mass = _cd('electron mass')
m_p = proton_mass = _cd('proton mass')
m_n = neutron_mass = _cd('neutron mass')
m_u = u = atomic_mass = _cd('atomic mass constant')

<<<<<<< HEAD
#angle in rad
=======
# angle in rad
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
degree = pi / 180
arcmin = arcminute = degree / 60
arcsec = arcsecond = arcmin / 60

<<<<<<< HEAD
#time in second
=======
# time in second
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
minute = 60.0
hour = 60 * minute
day = 24 * hour
week = 7 * day
year = 365 * day
Julian_year = 365.25 * day

<<<<<<< HEAD
#length in meter
=======
# length in meter
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
inch = 0.0254
foot = 12 * inch
yard = 3 * foot
mile = 1760 * yard
mil = inch / 1000
<<<<<<< HEAD
pt = point = inch / 72 #typography
=======
pt = point = inch / 72  # typography
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
survey_foot = 1200.0 / 3937
survey_mile = 5280 * survey_foot
nautical_mile = 1852.0
fermi = 1e-15
angstrom = 1e-10
micron = 1e-6
au = astronomical_unit = 149597870691.0
light_year = Julian_year * c
parsec = au / arcsec

<<<<<<< HEAD
#pressure in pascal
=======
# pressure in pascal
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
atm = atmosphere = _cd('standard atmosphere')
bar = 1e5
torr = mmHg = atm / 760
psi = pound * g / (inch * inch)

<<<<<<< HEAD
#area in meter**2
hectare = 1e4
acre = 43560 * foot**2

#volume in meter**3
litre = liter = 1e-3
gallon = gallon_US = 231 * inch**3 #US
#pint = gallon_US / 8
fluid_ounce = fluid_ounce_US = gallon_US / 128
bbl = barrel = 42 * gallon_US #for oil

gallon_imp = 4.54609e-3 #uk
fluid_ounce_imp = gallon_imp / 160

#speed in meter per second
kmh = 1e3 / hour
mph = mile / hour
mach = speed_of_sound = 340.5 #approx value at 15 degrees in 1 atm. is this a common value?
knot = nautical_mile / hour

#temperature in kelvin
zero_Celsius = 273.15
degree_Fahrenheit = 1/1.8 #only for differences

#energy in joule
eV = electron_volt = elementary_charge # * 1 Volt
=======
# area in meter**2
hectare = 1e4
acre = 43560 * foot**2

# volume in meter**3
litre = liter = 1e-3
gallon = gallon_US = 231 * inch**3  # US
# pint = gallon_US / 8
fluid_ounce = fluid_ounce_US = gallon_US / 128
bbl = barrel = 42 * gallon_US  # for oil

gallon_imp = 4.54609e-3  # UK
fluid_ounce_imp = gallon_imp / 160

# speed in meter per second
kmh = 1e3 / hour
mph = mile / hour
mach = speed_of_sound = 340.5  # approx value at 15 degrees in 1 atm. is this a common value?
knot = nautical_mile / hour

# temperature in kelvin
zero_Celsius = 273.15
degree_Fahrenheit = 1/1.8  # only for differences

# energy in joule
eV = electron_volt = elementary_charge  # * 1 Volt
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
calorie = calorie_th = 4.184
calorie_IT = 4.1868
erg = 1e-7
Btu_th = pound * degree_Fahrenheit * calorie_th / gram
Btu = Btu_IT = pound * degree_Fahrenheit * calorie_IT / gram
ton_TNT = 1e9 * calorie_th
<<<<<<< HEAD
#Wh = watt_hour

#power in watt
hp = horsepower = 550 * foot * pound * g

#force in newton
dyn = dyne = 1e-5
lbf = pound_force = pound * g
kgf = kilogram_force = g # * 1 kg

#functions for conversions that are not linear
=======
# Wh = watt_hour

# power in watt
hp = horsepower = 550 * foot * pound * g

# force in newton
dyn = dyne = 1e-5
lbf = pound_force = pound * g
kgf = kilogram_force = g  # * 1 kg

# functions for conversions that are not linear

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def C2K(C):
    """
    Convert Celsius to Kelvin

    Parameters
    ----------
<<<<<<< HEAD
    C : float-like scalar or array-like
        Celsius temperature(s) to be converted

    Returns
    -------
    K : float or a numpy array of floats, corresponding to type of Parameters
        Equivalent Kelvin temperature(s)

    Notes
    -----
    Computes `K = C +` `zero_Celsius` where `zero_Celsius` = 273.15, i.e.,
=======
    C : array_like
        Celsius temperature(s) to be converted.

    Returns
    -------
    K : float or array of floats
        Equivalent Kelvin temperature(s).

    Notes
    -----
    Computes ``K = C + zero_Celsius`` where `zero_Celsius` = 273.15, i.e.,
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    (the absolute value of) temperature "absolute zero" as measured in Celsius.

    Examples
    --------
<<<<<<< HEAD
    >>> from scipy.constants.constants import C2K
=======
    >>> from scipy.constants import C2K
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    >>> C2K(np.array([-40, 40.0]))
    array([ 233.15,  313.15])

    """
<<<<<<< HEAD
    return C + zero_Celsius
=======
    return _np.asanyarray(C) + zero_Celsius

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def K2C(K):
    """
    Convert Kelvin to Celsius

    Parameters
    ----------
<<<<<<< HEAD
    K : float-like scalar or array-like
        Kelvin temperature(s) to be converted

    Returns
    -------
    C : float or a numpy array of floats, corresponding to type of Parameters
        Equivalent Celsius temperature(s)

    Notes
    -----
    Computes `C = K -` `zero_Celsius` where `zero_Celsius` = 273.15, i.e.,
=======
    K : array_like
        Kelvin temperature(s) to be converted.

    Returns
    -------
    C : float or array of floats
        Equivalent Celsius temperature(s).

    Notes
    -----
    Computes ``C = K - zero_Celsius`` where `zero_Celsius` = 273.15, i.e.,
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    (the absolute value of) temperature "absolute zero" as measured in Celsius.

    Examples
    --------
<<<<<<< HEAD
    >>> from scipy.constants.constants import K2C
=======
    >>> from scipy.constants import K2C
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    >>> K2C(np.array([233.15, 313.15]))
    array([-40.,  40.])

    """
<<<<<<< HEAD
    return K - zero_Celsius
=======
    return _np.asanyarray(K) - zero_Celsius

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def F2C(F):
    """
    Convert Fahrenheit to Celsius

    Parameters
    ----------
<<<<<<< HEAD
    F : float-like scalar or array-like
        Fahrenheit temperature(s) to be converted

    Returns
    -------
    C : float or a numpy array of floats, corresponding to type of Parameters
        Equivalent Celsius temperature(s)

    Notes
    -----
    Computes `C = (F - 32) / 1.8`

    Examples
    --------
    >>> from scipy.constants.constants import F2C
=======
    F : array_like
        Fahrenheit temperature(s) to be converted.

    Returns
    -------
    C : float or array of floats
        Equivalent Celsius temperature(s).

    Notes
    -----
    Computes ``C = (F - 32) / 1.8``.

    Examples
    --------
    >>> from scipy.constants import F2C
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    >>> F2C(np.array([-40, 40.0]))
    array([-40.        ,   4.44444444])

    """
<<<<<<< HEAD
    return (F - 32) / 1.8
=======
    return (_np.asanyarray(F) - 32) / 1.8

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def C2F(C):
    """
    Convert Celsius to Fahrenheit

    Parameters
    ----------
<<<<<<< HEAD
    C : float-like scalar or array-like
        Celsius temperature(s) to be converted

    Returns
    -------
    F : float or a numpy array of floats, corresponding to type of Parameters
        Equivalent Fahrenheit temperature(s)

    Notes
    -----
    Computes `F = 1.8 * C + 32`

    Examples
    --------
    >>> from scipy.constants.constants import C2F
=======
    C : array_like
        Celsius temperature(s) to be converted.

    Returns
    -------
    F : float or array of floats
        Equivalent Fahrenheit temperature(s).

    Notes
    -----
    Computes ``F = 1.8 * C + 32``.

    Examples
    --------
    >>> from scipy.constants import C2F
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    >>> C2F(np.array([-40, 40.0]))
    array([ -40.,  104.])

    """
<<<<<<< HEAD
    return 1.8 * C + 32
=======
    return 1.8 * _np.asanyarray(C) + 32

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def F2K(F):
    """
    Convert Fahrenheit to Kelvin

    Parameters
    ----------
<<<<<<< HEAD
    F : float-like scalar or array-like
        Fahrenheit temperature(s) to be converted

    Returns
    -------
    K : float or a numpy array of floats, corresponding to type of Parameters
        Equivalent Kelvin temperature(s)

    Notes
    -----
    Computes `K = (F - 32)/1.8 +` `zero_Celsius` where `zero_Celsius` =
=======
    F : array_like
        Fahrenheit temperature(s) to be converted.

    Returns
    -------
    K : float or array of floats
        Equivalent Kelvin temperature(s).

    Notes
    -----
    Computes ``K = (F - 32)/1.8 + zero_Celsius`` where `zero_Celsius` =
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    273.15, i.e., (the absolute value of) temperature "absolute zero" as
    measured in Celsius.

    Examples
    --------
<<<<<<< HEAD
    >>> from scipy.constants.constants import F2K
=======
    >>> from scipy.constants import F2K
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    >>> F2K(np.array([-40, 104]))
    array([ 233.15,  313.15])

    """
<<<<<<< HEAD
    return C2K(F2C(F))
=======
    return C2K(F2C(_np.asanyarray(F)))

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def K2F(K):
    """
    Convert Kelvin to Fahrenheit

    Parameters
    ----------
<<<<<<< HEAD
    K : float-like scalar or array-like
        Kelvin temperature(s) to be converted

    Returns
    -------
    F : float or a numpy array of floats, corresponding to type of Parameters
        Equivalent Fahrenheit temperature(s)

    Notes
    -----
    Computes `F = 1.8 * (K -` `zero_Celsius` `) + 32` where `zero_Celsius` =
=======
    K : array_like
        Kelvin temperature(s) to be converted.

    Returns
    -------
    F : float or array of floats
        Equivalent Fahrenheit temperature(s).

    Notes
    -----
    Computes ``F = 1.8 * (K - zero_Celsius) + 32`` where `zero_Celsius` =
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    273.15, i.e., (the absolute value of) temperature "absolute zero" as
    measured in Celsius.

    Examples
    --------
<<<<<<< HEAD
    >>> from scipy.constants.constants import K2F
=======
    >>> from scipy.constants import K2F
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    >>> K2F(np.array([233.15,  313.15]))
    array([ -40.,  104.])

    """
<<<<<<< HEAD
    return C2F(K2C(K))

#optics
=======
    return C2F(K2C(_np.asanyarray(K)))

# optics

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def lambda2nu(lambda_):
    """
    Convert wavelength to optical frequency

    Parameters
    ----------
<<<<<<< HEAD
    lambda : float-like scalar or array-like
        Wavelength(s) to be converted

    Returns
    -------
    nu : float or a numpy array of floats, corresponding to type of Parameters
        Equivalent optical frequency(ies)

    Notes
    -----
    Computes :math:`\\nu = c / \\lambda` where `c` = 299792458.0, i.e., the
=======
    lambda_ : array_like
        Wavelength(s) to be converted.

    Returns
    -------
    nu : float or array of floats
        Equivalent optical frequency.

    Notes
    -----
    Computes ``nu = c / lambda`` where c = 299792458.0, i.e., the
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    (vacuum) speed of light in meters/second.

    Examples
    --------
<<<<<<< HEAD
    >>> from scipy.constants.constants import lambda2nu
=======
    >>> from scipy.constants import lambda2nu, speed_of_light
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    >>> lambda2nu(np.array((1, speed_of_light)))
    array([  2.99792458e+08,   1.00000000e+00])

    """
<<<<<<< HEAD
    return c / lambda_
=======
    return _np.asanyarray(c) / lambda_

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def nu2lambda(nu):
    """
    Convert optical frequency to wavelength.

    Parameters
    ----------
<<<<<<< HEAD
    nu : float-like scalar or array-like
        Optical frequency(ies) to be converted

    Returns
    -------
    lambda : float or a numpy array of floats, corresp. to type of Parameters
        Equivalent wavelength(s)

    Notes
    -----
    Computes :math:`\\lambda = c / \\nu` where `c` = 299792458.0, i.e., the
=======
    nu : array_like
        Optical frequency to be converted.

    Returns
    -------
    lambda : float or array of floats
        Equivalent wavelength(s).

    Notes
    -----
    Computes ``lambda = c / nu`` where c = 299792458.0, i.e., the
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    (vacuum) speed of light in meters/second.

    Examples
    --------
<<<<<<< HEAD
    >>> from scipy.constants.constants import nu2lambda
=======
    >>> from scipy.constants import nu2lambda, speed_of_light
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    >>> nu2lambda(np.array((1, speed_of_light)))
    array([  2.99792458e+08,   1.00000000e+00])

    """
<<<<<<< HEAD
    return c / nu
=======
    return c / _np.asanyarray(nu)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
