#!/usr/bin/env python
##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Translator.py
# Class for Translator.
#
# Author: mathwig
# ----------------------------------------------------------------------------
import string

LanguageTables = {
    "english" : {},
    "french" : {
        "Expressions" : [
            ('mostly sunny', 'generalement ensoleille'),
            ('mostly clear', 'generalement degage'),
            ('mostly cloudy', 'generalement nuageux'),
            ('partly cloudy', 'partiellement nuageux'),
            ('sunny', 'ensoleille'),
            ('clear', 'ciel degage'),
            ('cloudy', 'nuageux'),
            ('high winds', 'vents forts'),
            ('light winds', 'vents faibles'),
            ('very windy', 'tres venteux'),
            ('probability of precipitation', 'probabilite de precipitations'),
            ('chance of precipitation', 'risque de precipitations'),
            ('areal coverage of precipitation', 'areal coverage de precipitations'),
            ('slight chance of', 'faible risque de'),
            ('chance of', 'risque de'),
            ('snow accumulation of', 'accumulation de neige de'),
            ('northeast winds at', 'vents du nord-est de'),
            ('northwest winds at', 'vents du nord-ouest de'),
            ('southest winds at', 'vents du sud-est de'),
            ('southwest winds at', 'vents du sud-ouest de'),
            ('east winds at', "vents de l'est de"),
            ('west winds at', "vents de l'ouest de"),
            ('north winds at', 'vents du nord de'),
            ('south winds at', 'vents du sud de'),
            ('northeast winds up', "vents du nord-est jusqu'"),
            ('northwest winds up', "vents du nord-ouest jusqu'"),
            ('southest winds up', "vents du sud-est jusqu'"),
            ('southwest winds up', "vents du sud-ouest jusqu'"),
            ('east winds up', "vents de l'est jusqu'"),
            ('west winds up', "vents de l'ouest jusqu'"),
            ('north winds up', "vents du nord jusqu'"),
            ('south winds up', "vents du sud jusqu'"),
            ('northeast', 'nord-est'),
            ('northwest', 'nord-ouest'),
            ('southeast', 'sud-est'),
            ('southwest', 'sud-ouest'),
            ('east', 'est'),
            ('west', 'ouest'),
            ('north', 'nord'),
            ('south', 'sud'),
            ('in the evening', 'au cours de la soiree'),
            ('in the night', 'au cours de la nuit'),
            ('in the morning', 'au cours de la matinee'),
            ('in the afternoon', "au cours de l'apres-midi"),
            ('through the evening', 'pret de la soiree'),
            ('through the night', 'pret de la nuit'),
            ('through the morning', 'pret de la matinee'),
            ('through the afternoon', "pret de l'apres-midi"),
            ('through', "pret"),
            ('overnight', 'pendant la nuit'),
            ('decreasing to', 'changeant a'),
            ('increasing to', 'augmentant a'),
            ('shifting to the', 'devenant du'),
            ('becoming', 'devenant'),
            ('much warmer', 'beaucoup plus chaud'),
            ('warmer','plus chaud'),
            ('cooler','plus frais'),
            ('sharply colder', 'beaucoup plus froid'),
            ('clearing later', 'se degageant plus tard'),
            ('becoming cloudy', 'devenant nuageux'),
            ('increasing cloudiness', 'augmentation de nuages'),
            ('decreasing cloudiness', 'diminution de nuages'),
            ('around', 'de pres de'),
            ('in the lower', 'dans les bas'),
            ('in the mid', 'dans les mi-'),
            ('in the upper', 'dans les hauts'),
            ('in the', 'dans les'),
            ('highs', 'maximums'),
            ('lows', 'minimums'),
            ('. high', '. maximum'),
            ('. low', '. minimum'),
            ('rising', 'montant'),
            ('falling', 'descendant'),
            ('high', 'eleve'),
            ('low', 'bas'),
            ('temperatures', 'temperaturas'),
            ('percent', 'pour cent'),
            ('inches', 'pouces'),
            ('inch', 'pouce'),
            (' to ', ' a '),
            (' at ', ' a '),
            (' and ', ' et '),
            ('mixed with', 'avec'),
            ('with', 'avec'),
            ('no swells', 'aucune houle'),
            ('swell', 'houle'),
            ('waves', 'mer'),
            ('less than', 'de moins de'),
            ('sky/weather...', 'CIEL/METEO.......'),
            ('lal...........', 'LAL..............'),
            ('temperature...', 'TEMPERATURE......'),
            ('humidity......', 'HUMIDITE.........'),
            ('wind - 20 ft..', 'VENT - 20 PIEDS..'),
            ('valleys...', 'VALLEES...'),
            ('ridges....', 'ARETES....'),
            ('haines index..', 'INDICE HAINES....'),
            ('smoke dispersal', 'DISPERSION DE FUMEE'),
            ('mixing height...', 'HAUTEUR DE MELANGE..'),
            ('transport wind..', 'VENT DE TRANSPORTATION..'),
            ('visibility','visibilite'),
            ('frequent lightning','foudre frequente'),
            ('gusty winds','vents brisques'),
            ('heavy rainfall','pluie abondante'),
            ('damaging winds','vents damageux'),
            ('small hail','petite grele'),
            ('large hail','grosse grele'),
            ('then', 'alors'),
            ],
        "Types" : [
            ('freezing rain', 'pluie verglacante', 'FS'),
            ('rain showers', 'averses de pluie', 'FP'),
            ('rain', 'pluie', 'FS'),
            ('freezing drizzle', 'bruine', 'FS'),
            ('drizzle', 'bruine', 'FS'),
            ('snow showers', 'averses de neige', 'FP'),
            ('snow', 'neige', 'FS'),
            ('dust', 'poussiere', 'FS'),
            ('fog', 'brouillard', 'MS'),
            ('haze', 'brume', 'FS'),
            ('hail', 'grele', 'FS'),
            ('sleet', 'verglas', 'MS'),
            ('smoke', 'fumee', 'FS'),
            ('thunderstorms', 'orages', 'MP'),
            ('volcanic ash', 'cendre volcanique', 'FS')
            ],
        "Intensities" :[
            ('very light', 'tres faible', 'tres faible', 'tres faible',
             'tres faibles'),
            ('light', 'faible', 'faibles', 'faible', 'faibles'),
            ('moderate', 'modere', 'moderes', 'moderee', 'moderees'),
            ('very heavy', 'tres abondant', 'tres abondants',
             'tres abondante', 'tres abondantes'),
            ('heavy', 'abondant', 'abondants', 'abondante', 'abondantes')
            ],
        "Coverages" : [
            ('isolated', 'isole', 'isoles', 'islolee', 'isolees'),
            ('widely scattered', 'largement disperse',
             'largement disperses',
              'largement dispersee', 'largement dispersees'),
            ('scattered', 'disperse', 'disperses', 'dispersee',
             'dispersees'),
            ('widespread', 'repandu', 'repandus', 'repanduee', 'repanduees'),
            ('occasional', 'passage', 'passages', 'passagee', 'passagees'),
            ('numerous','nombreux','nombreux','nombreuse','nombreuses')
            ],
        "Exceptions" : [
            ('likely', 'probable', 'probables', 'probable', 'probables')
            ],
        "CleanUp" :  [
            ('de a', "d'a"),
            ('mi- ', 'mi-'),
            ("jusqu' a", "jusqu'a")
            ]
        },
    "spanish" : {
        "Expressions" : [
            ('mostly sunny', 'mayormente soleado'),
            ('mostly clear', 'mayormente claro'),
            ('mostly cloudy', 'mayormente nublado'),
            ('partly cloudy', 'parcialmente nublado'),
            ('sunny', 'soleado'),
            ('clearing','despejandose'),
            ('clear', 'despejado'),
            ('later','mas tarde'),
            ('cloudy', 'nublado'),
            ('snow accumulation of', 'acumulacion de nieve'),
            ('probability of precipitation', 'probabilidad de lluvias'),
            ('chance of precipitation', 'probabilidad de lluvias'),
            ('areal coverage of precipitation', 'areal coverage de lluvias'),
            ('slight chance of', 'leve probabilidad de'),
            ('chance of', 'probabilidad de'),
            ('in the night', 'en la noche'),
            ('during the night','durante la noche'),
            ('in the morning', 'en la manana'),
            ('in the afternoon', 'en la tarde'),
            ('in the evening', 'temprano en la noche'),
            ('through the morning', 'recorriendo la manana'),
            ('through the afternoon', 'recorriendo la tarde'),
            ('through the evening', 'recorriendo la temprano noche'),
            ('through', 'recorriendo'),
            ('overnight', 'durante la noche'),
            ('early evening','temprano en la noche'),
            ('evening','temprano en la noche'),
            ('decreasing to', 'disminuyendo'),
            ('increasing to', 'aumentando'),
            ('shifting to the', 'tornandose del'),
            ('becoming', 'tornandose'),
            ('then','luego'),
            ('followed by','seguido por'),
            ('much warmer', 'mucho mas caliente'),
            ('warmer', 'mas caliente'),
            ('sharply colder', 'marcadamente frio'),
            ('cooler', 'mas fresco'),
            ('clearing later', 'aclrarando tarde'),
            ('becoming cloudy', 'llegando a ser nublado'),
            ('increasing cloudiness', 'nubosidad aumentando'),
            ('decreasing cloudiness', 'nubosidad disminuyendo'),
            ('high winds', 'vientos fuertes'),
            ('. highs', '. temperaturas maximas'),
            ('. lows', '. temperaturas minimas'),
            ('northeast winds', 'vientos del noreste'),
            ('northwest winds', 'vientos del noroeste'),
            ('southeast winds', 'vientos del sureste'),
            ('southwest winds', 'vientos del suroeste'),
            ('east winds', 'vientos del este'),
            ('west winds', 'vientos del oeste'),
            ('north winds', 'vientos del norte'),
            ('south winds', 'vientos del sur'),
            ('northeast winds up', 'vientos del noreste hasta de'),
            ('northwest winds up', 'vientos del noroeste hasta de'),
            ('southest winds up', 'vientos del sureste hasta de'),
            ('southwest winds up', 'vientos del suroeste hasta de'),
            ('east winds up', 'vientos del este hasta de'),
            ('west winds up', 'vientos del oeste hasta de'),
            ('north winds up', 'vientos del norte hasta de'),
            ('south winds up', 'vientos del sur hasta de'),
            ('northeast', 'noreste'),
            ('northwest', 'noroeste'),
            ('southeast', 'sureste'),
            ('southwest', 'suroeste'),
            ('small craft exercise caution','precaucion a embarcaciones pequenas'),
            ('small craft advisory','advertencias a embarcaciones pequenas'),
            ('knots','nudos'),
            ('seas','mares de'),
            ('bay and inland waters','aguas tierra adentro y de las bahias'),
            ('inland waters','aguas tierra adentro'),
            ('a light chop','picadas ligeramente'),
            ('a moderate chop','picadas moderadamente'),
            ('rough','turbulentas'),
            ('very rough','bien turbulentas'),
            ('almost smooth','casi llanas'),
            ('smooth','llanas'),
            ('choppy','picadas'),
            ('extended forecast','pronostico extendido'),
            ('forecast for', 'pronostico de'), 
            ('bay waters','aguas de la bahia'),
            ('lake waters','aguas del lago'),
            ('feet','pies'),
            ('foot','pie'),
            ('east', 'este'),
            ('west', 'oeste'),
            ('north', 'norte'),
            ('south', 'sur'),
            ('patchy dense fog','niebla densa esparcida'),
            ('areas of dense fog','areas de niebla densa'),
            ('widespread dense fog','niebla densa extensa'),
            ('patchy fog','niebla esparcida'),
            ('areas of fog','areas de niebla'),
            ('widespread fog','niebla extensa'),
            ('dense fog','niebla densa'),
            ('around', 'alrededor de'),
            ('in the lower to mid', 'en los bajos a medios'),
            ('in the mid to upper', 'en los medios a altos'),
            ('in the lower', 'en los bajos'),
            ('in the mid', 'en los medios'),
            ('in the upper', 'en los altos'),
            ('in the', 'en los'),
            ('low', 'bajo'),
            ('high', 'alto'),
            ('no swells', 'no marejeda'),
            ('swell', 'marejeda'),
            ('waves', 'ondas'),
            ('less than', 'menos de'),
            ('percent', 'por ciento'),
            ('inches', 'pulgadas'),
            ('inch', 'pulgada'),
            ('light winds', 'vientos ligeros'),
            ('very windy', 'muy ventoso'),
            ('windy', 'ventoso'),
            ('breezy','brisas'),
            ('and gusty','con rafagas mas altas'),
            (' and ', ' y '),
            (' to ', ' a '),
            (' at ', ' en '),
            (' with ',' con '),
            ('mixed with', 'con'),
            ('sky/weather...', 'CIELO/TIEMPO.....'),
            ('lal...........', 'NAE..............'),
            ('temperature...', 'TEMPERATURA......'),
            ('humidity......', 'HUMEDAD........'),
            ('wind - 20 ft..', 'VIENTO - 20 FT..'),
            ('valleys...', 'VALLES...'),
            ('ridges....', 'CRESTAS....'),
            ('haines index..', 'INDICE DE HAINES....'),
            ('smoke dispersal', 'DISPERSION DE HUMO'),
            ('mixing height...', 'ALTURA DE MEZCLA..'),
            ('transport wind..', 'VIENTO TRANSPORTADOR..'),
            ('visibility','visibilidad'),
            ('frequent lightning','rayos frequentes'),
            ('gusty winds','rachas de viento'),
            ('heavy rainfall','lluvia intensa'),
            ('damaging winds','vientos perjudiciales'),
            ('small hail','granizo pequeno'),
            ('large hail','granizo grande'),
            ('sunday night','domingo por la noche'),
            ('monday night','lunes por la noche'),
            ('tuesday night','martes por la noche'),
            ('wednesday night','miercoles por la noche'),
            ('thursday night','jueves por la noche'),
            ('friday night','viernes por la noche'),
            ('saturday night','sabado por la noche'),
            ('sunday','domingo'),
            ('monday','lunes'),
            ('tuesday','martes'),
            ('wednesday','miercoles'),
            ('thursday','jueves'),
            ('friday','viernes'),
            ('saturday','sabado'),
            ('tonight','esta noche'),
            ('today','hoy'),
            ('scattered thunderstorms','tormentas dispersas'),
            ('isolated thunderstorms','tormentas aisladas'),
            ('near the coast','cerca de la costa'),
            ('inland','tierra adentro'),
            ('winds','vientos'),
            ('wind','vientos'),
            ('or less','o menos')
            ],
        "Types" : [
            ('freezing rain', 'lluvia helada', 'FS'),
            ('rain showers', 'chubascos', 'MP'),
            ('showers', 'chubascos', 'MP'),
            ('freezing drizzle', 'llovizna helada', 'FS'),
            ('rain', 'lluvia', 'FS'),
            ('drizzle', 'llovizna', 'FS'),
            ('snow showers', 'chuvascos de nieve', 'FS'),
            ('snow', 'nieve', 'FS'),
            ('fog', 'nieblas', 'MS'),
            ('dust', 'polvo', 'MS'),
            ('haze', 'neblina', 'FS'),
            ('hail', 'granizo', 'MS'),
            ('sleet', 'aguanieve', 'FS'),
            ('smoke', 'humo', 'MS'),
            ('thunderstorms', 'tormentas', 'MP'),
            ('volcanic ash', 'ceniza volcanica', 'FS')
            ],
        "Intensities" : [
            ('light', 'muy ligero', 'muy ligeros', 'muy ligera',
             'muy ligeras'),
            ('light', 'ligero', 'ligero', 'ligero', 'ligero'),
            ('moderate', 'moderado', 'moderado', 'moderado', 'moderado'),
            ('very heavy', 'muy intenso', 'muy intensos', 'muy intensa',
             'muy intensas'),
            ('heavy', 'intenso', 'intensos', 'intensa', 'intensas'),
            ('numerous','numeroso','numerosos','numerosa','numerosas')
            ],
        "Coverages" : [
            ('isolated', 'aislado', 'aislados', 'aislada', 'aisladas'),
            ('widely scattered', 'extensamente disperso',
             'extensamente dispersos', 'extensamente dispersa',
             'extensamente dispersas'),
            ('scattered', 'disperso', 'dispersos', 'dispersa', 'dispersas'),
            ('occasional', 'ocasional', 'ocasionales', 'ocasional',
             'ocasionales'),
            ('widespread', 'muy difundido', 'muy difundidos',
             'muy difundida','muy difundidas')
            ],
        "Exceptions" :[
            ('likely', 'probable', 'probables', 'probable', 'probables')
            ],
       "CleanUp" :[ ]
        }
    }


# ValueDict :  This index and table contain subsitution values for the HTML
#   Template pages, FastWx.html and Table.html
Index = {
    "english": 1,
    "french" : 2,
    "spanish": 3
}

ValueDictTable = [
    ("Type", "Category", "Categorie", "Categoria"),
    ("Public", "Public", "Publiques", "Publico"),
    ("FireWeather", "FireWeather", "Previsions Feu", "Incendios-Tiempo"),
    ("Aviation", "Aviation", "Aviation", "Aviacion"),
    ("Marine", "Marine", "Marine", "Marino"),
    ("Language", "Language", "Langue", "Lenguaje"),
    ("Audio", "Audio", "Audio", "Audio"),
    ("Click", "CLICK", "CLIQUER", "HAGA CLIC"),
    ("ClickText", "on a location OR Select:",
     "sur une location ou Choisir",
     "en una localidad o Seleccionela"),
    ("CityTable", "Table of Cities", "Table de Villes", "Tabla de Ciudades"),
    ("CountyTable", "Table of Counties", "Table de Comtes",
      "Tabla de Condados "),
    ("issued", "Issued", "Emises", "Emitido"),
    ("Site","Forecast Location: ","Site de Previsions: ",
      "Terreno de Pronostico: "),
    ("English", "English", "Anglais", "Ingles"),
    ("Spanish", "Spanish", "Espagnol", "Espanol"),
    ("French", "French", "Francais", "Franceses")
    ]

# General Expressions: Time, Column headings, Web page
Expression = [
    ("Tonight", "Ce soir", "Esta Noche"),
    ('Today', "Aujourd'hui", "Hoy"),
    ('Night', "soir", "Noche"),
    ('Monday',"Lundi","Lunes"),
    ('Tuesday',"Mardi", "Martes"),
    ('Wednesday',"Mercredi", "Miercoles"),
    ('Thursday',"Jeudi","Jueves"),
    ('Friday',"Vendredi","Viernes"),
    ('Saturday',"Samedi","Sabado"),
    ('Sunday',"Dimanche","Domingo"),
    ("TONIGHT", "CE SOIR", "ESTA NOCHE"),
    ('TODAY', "AUJOURD'HUI", "HOY"),
    ('NIGHT', "SOIR", "NOCHE"),
    ('MONDAY',"LUNDI","LUNES"),
    ('TUESDAY',"MARDI", "MARTES"),
    ('WEDNESDAY',"MERCREDI", "MIERCOLES"),
    ('THURSDAY',"JEUDI","JUEVES"),
    ('FRIDAY',"VENDREDI","VIERNES"),
    ('SATURDAY',"SAMEDI","SABADO"),
    ('SUNDAY',"DIMANCHE","DOMINGO"),
    ('Jan', "Jan", "Erno"),
    ('Feb', "Fev", "Febrero"),
    ('Mar', "Mar", "Marzo"),
    ('Apr',"Avr","Abril"),
    ('May',"Mai","Mayo"),
    ('Jun',"Juin","Junio"),
    ('Jul',"Juil","Julio"),
    ('Aug',"Aout", "Agosto"),
    ('Sep',"Sep","Septiembre"),
    ('Oct',"Oct","Octubre"),
    ('Nov',"Nov","Noviembre"),
    ('Dec',"Dec","Diciembre"),
    ('Sky','Ciel','Cielo'),
    ('Wind (mph)','Vent (mph)','Viento (mph)'),
    ('Max Temp','Temp Max','Temp Max'),
    ('Min Temp','Temp Min','Temp Min'),
    ('Precip','Precip','Lluvias'),
    ('Wind (kt)','Vent (kt)','Viento (kt)'),
    ('Waves (ft)','Vagues (pd)','Ondas (ft)'),
    ('Swells (ft)','Houles (ft)','Swells (ft)'),
    ('LAL','LAL','LAL'),
    ('RelHum(%)','HumRel(%)','RelHum(%)'),
    ('MaxT','TMax','TMax',),
    ('MinT','TMin','TMin'),
    ('FreeWind(mph)','VentLibre(mph)','VientoLibre(mph)'),
    ('Haines','Haines','Haines'),
    ('TransWind(mph)','VentTrans(mph)','VientoTrans(mph)'),
    ('MixHgt(ft agl)','ElevMelang(ft agl)','AltuMezcl(ft agl)'),
    ('City','Ville', 'Ciudad'),
    ('County','Comte', 'Condado'),
    ('Nowcast','Previsions Courantes','Pronostico Sobre Tiempo'),
    ('Short Term Forecast','Previsions Court Terme',\
      'Pronostico a Corto Plazo'),
    ('Extended Forecast','Previsions Long Terme','Pronostico a Largo Plazo'),
    ('Spot Forecast','Previsions Spot','Pronostico Spot'),
    ('Outlook','Perspective','Panorama'),
    ('Marine Nowcast','Previsions Marines Courantes',
      'Pronostico Sobre-Tiempo Maritimo'),
    ('Coastal Marine Forecast','Coastal Marine Forecast',
      'Pronostico Maritimo Costero'),
    ('Terminal Aerodrome Forecast',"Previsions a l'Aerodrome",
      'Pronostico Para Terminal Aerodromo'),
    ('Latitude','Latitude','Latitud'),
    ('Longitude','Longitude','Longitud'),
    ('Area','Aire','Area'),
    ('Cities','Villes','Ciudades'),
    ('Counties','Comtes','Condados'),
    ('in the morning','du matin','por la manana'),
    ('in the afternoon',"de l'apres-midi", "por la tarde"),
    ('in the evening','du soir',"por la tarde"),
    ('during the night','pendant la nuit','durante la noche'),
    ('followed by', 'suivi par', 'seguido poru'),
    ('overnight', 'pendant la nuit', 'durante la noche'),
    ]

class Translator:
    def __init__(self, language, parent=None):

        self._language = language
        self._langDict = LanguageTables[language]

    # Function translating a forecast
    def getForecast(self, forecast):
        lwForecast = string.lower(forecast)

        # Convert forecast using translation tuples
        transForecast = self._translateExpForecast(lwForecast)

        # Convert the exceptions
        exceptForecast = self._translateExceptions(transForecast)

        # Convert forecast using type and intensity tuples
        transForecast = self._translateTypeForecast(exceptForecast)

        # Clean up the translated forecast
        cleanTransForecast = self._cleanUp(transForecast)

        # Capitalize the beginning of sentences
        self.capTransForecast = self._capital(cleanTransForecast)

        return self.capTransForecast

    # Function converting appropriate letters of a string to capital letters
    def _capital(self, str):

        if str == "":
            return str

        # Find all the periods
        index = []

        for i in range(0, len(str)-1):
            if str[i] == "." and str[i+1] == " ":
                index.append(i+2)
            elif str[i] == "." and str[i+1] == ".":
                index.append(i+2)

        # Always capitalize the first letter
        capitalStr = string.upper(str[0])

        # Capitalize the letters following the periods and a space
        for i in range(1, len(str)):
            if i in index:
                capitalStr = capitalStr + string.upper(str[i])
            else:
                capitalStr = capitalStr + str[i]

        return capitalStr


    # Function translating a forecast using the translation expression tuples
    def _translateExpForecast(self, lwForecast):

        for expr, transExpr in self._langDict['Expressions']:
            #print expr, transExpr
            lwForecast = string.replace(lwForecast, expr, transExpr)

        return lwForecast

    # Function translating a forecast using the translation type and
    # intensity tuples
    def _translateTypeForecast(self, lwForecast):

        # translate combination of type, intensity, and coverage
        for ttuple in self._langDict['Types']:
            for ituple in self._langDict['Intensities']:
                for ctuple in self._langDict['Coverages']:
                    origEx = ctuple[0] + ' ' + ituple[0] + ' ' + ttuple[0]
                    transEx = ''
                    if ttuple[2] == 'MS':
                        transEx = ttuple[1] + ' ' + ituple[1] + ' ' + ctuple[1]
                    elif ttuple[2] == 'MP':
                        transEx = ttuple[1] + ' ' + ituple[2] + ' ' + ctuple[2]
                    elif ttuple[2] == 'FS':
                        transEx = ttuple[1] + ' ' + ituple[3] + ' ' + ctuple[3]
                    elif ttuple[2] == 'FP':
                        transEx = ttuple[1] + ' ' + ituple[4] + ' ' + ctuple[4]
                    lwForecast = string.replace(lwForecast, origEx, transEx)

        # translate combination of type and intensity (no coverage)
        for ttuple in self._langDict['Types']:
            for ituple in self._langDict['Intensities']:
                origEx = ituple[0] + ' ' + ttuple[0]
                transEx = ''
                if ttuple[2] == 'MS':
                    transEx = ttuple[1] + ' ' + ituple[1]
                elif ttuple[2] == 'MP':
                  transEx = ttuple[1] + ' ' + ituple[2]
                elif ttuple[2] == 'FS':
                  transEx = ttuple[1] + ' ' + ituple[3]
                elif ttuple[2] == 'FP':
                  transEx = ttuple[1] + ' ' + ituple[4]
                lwForecast = string.replace(lwForecast, origEx, transEx)

        # translate combination of type and coverage (no intensity)
        for ttuple in self._langDict['Types']:
            for ctuple in self._langDict['Coverages']:
                origEx = ctuple[0] + ' ' + ttuple[0]
                transEx = ''
                if ttuple[2] == 'MS':
                    transEx = ttuple[1] + ' ' + ctuple[1]
                elif ttuple[2] == 'MP':
                  transEx = ttuple[1] + ' ' + ctuple[2]
                elif ttuple[2] == 'FS':
                  transEx = ttuple[1] + ' ' + ctuple[3]
                elif ttuple[2] == 'FP':
                  transEx = ttuple[1] + ' ' + ctuple[4]
                lwForecast = string.replace(lwForecast, origEx, transEx)

        # translate type (no coverage and no intensity)
        for ttuple in self._langDict['Types']:
            lwForecast = string.replace(lwForecast, ttuple[0], ttuple[1])

        return lwForecast

    # Convert the exceptions
    def _translateExceptions(self, transForecast):
        for ttuple in self._langDict['Types']:
            for etuple in self._langDict['Exceptions']:
                origEx = ttuple[0] + ' ' + etuple[0]
                transEx = ''
                if ttuple[2] == 'MS':
                  transEx = ttuple[0] + ' ' + etuple[1]
                elif ttuple[2] == 'MP':
                  transEx = ttuple[0] + ' ' + etuple[2]
                elif ttuple[2] == 'FS':
                  transEx = ttuple[0] + ' ' + etuple[3]
                elif ttuple[2] == 'FP':
                  transEx = ttuple[0] + ' ' + etuple[4]
                transForecast = string.replace(transForecast, origEx, transEx)
        return transForecast

    # Function cleaning up the translated forecast
    def _cleanUp(self, lwForecast):

        for expr, transExpr in self._langDict['CleanUp']:
            lwForecast = string.replace(lwForecast, expr, transExpr)

        return lwForecast

    def getExpression(self, phrase):
        "Translate the phrase"

        if self._language == "english":
            return phrase
        index = Index[self._language] - 1
        for expr in Expression:
            phrase = string.replace(phrase, expr[0], expr[index])
        return phrase


if __name__ == '__main__':

    forecastList = [
    "High winds in the afternoon. Partly cloudy. Very heavy rain showers likely. Snow accumulation of 1 inch. Lows in the mid 30s. East winds at 75 mph. Probability of precipitation 65 percent.",
    "Mostly sunny. Widespread heavy volcanic ash. Snow accumulation of 1 to 20 inches. Highs around 120. Probability of precipitation 99 percent.",
    "High winds. Partly cloudy. Slight chance of very heavy rain showers. Snow accumulation of 1 inch. Lows in the mid 30s. East winds at 75 mph. Probability of precipitation 1 percent. Southwest winds up to 15 mph.",
    "SKY/WEATHER...Mostly cloudy with scattered rain showers and thunderstorms\nLAL...........3-4\nTEMPERATURE...Lows in the mid 30s\nHUMIDITY......70 pct\nWIND - 20 FT..Northwest to Southeast in the evening\n    VALLEYS...\n    RIDGES....\nHAINES INDEX..4 low\nSMOKE DISPERSAL:\n MIXING HEIGHT...Decreasing to 500-1,000 ft agl\n TRANSPORT WIND..Northeast to southeast 3-8 mph",
    "High winds. Decreasing cloudiness. Widely scattered light sleet. Snow accumulation of 1 to 50 inches. Low 0. Northwest winds at 90 to 100 mph becoming southwest at 80 to 90 mph. Probability of precipitation 110 percent." ]

    for forecast in forecastList:
        #transForecast = Translator('french')
        transForecast = Translator('spanish')
        print ' '
        print 'Original Forecast'
        print forecast
        print ' '
        print 'Translated Forecast'
        print transForecast.getForecast(forecast)
