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

from __future__ import print_function
import datetime
from dynamicserialize.dstypes.com.raytheon.uf.common.dataquery.requests import RequestConstraint
from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange
from ufpy.dataaccess import DataAccessLayer as DAL
from ufpy.ThriftClient import ThriftRequestException

import baseDafTestCase
import unittest

#
# Test DAF support for climate data
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/19/16        4795          mapeters       Initial Creation.
#    04/11/16        5548          tgurney        Cleanup
#    04/18/16        5548          tgurney        More cleanup
#    04/26/16        5587          tgurney        Add identifier values tests
#    06/09/16        5574          mapeters       Add advanced query tests, Short parameter test
#    06/13/16        5574          tgurney        Fix checks for None
#    06/21/16        5548          tgurney        Skip tests that cause errors
#    10/06/16        5926          dgilling       Add additional time and location tests.
#
#


class ClimateTestCase(baseDafTestCase.DafTestCase):
    """Test DAF support for climate data"""

    datatype = 'climate'

    def testGetAvailableParameters(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_asos_monthly')
        self.runParametersTest(req)

    def testGetAvailableLocations(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_asos_monthly')
        self.runLocationsTest(req)

    def testGetAvailableLocationsForRptTable(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.rpt')
        self.runLocationsTest(req)

    def testGetAvailableLocationsForStationId(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.day_climate_norm')
        self.runLocationsTest(req)

    def testGetAvailableLocationsForInformId(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_mon_season_yr')
        self.runLocationsTest(req)

    def testGetAvailableLocationsWithConstraints(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_asos_monthly')
        req.addIdentifier('maxtemp_mon', RequestConstraint.new('>', 95))
        self.runLocationsTest(req)

    def testGetAvailableLocationsWithInvalidTable(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.boolean_values')
        with self.assertRaises(ThriftRequestException) as cm:
            DAL.getAvailableLocationNames(req)
        self.assertIn('IncompatibleRequestException', str(cm.exception))

    def testGetAvailableTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_asos_monthly')
        req.setParameters('maxtemp_mon', 'min_sea_press')
        self.runTimesTest(req)

    def testGetAvailableTimesWithLocationNamesForYearMonth(self):
        """
        Test retrieval of times for a climo table that uses year and 
        month columns to build DataTimes.
        """
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_asos_monthly')
        req.setLocationNames('KOMA', 'KABR', 'KDMO')
        req.setParameters('maxtemp_mon', 'min_sea_press')
        self.runTimesTest(req)
        
    def testGetAvailableTimesWithLocationNamesForYearDayOfYear(self):
        """
        Test retrieval of times for a climo table that uses year and 
        day_of_year columns to build DataTimes.
        """
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_asos_daily')
        req.setLocationNames('KOMA', 'KABR', 'KDMO')
        req.setParameters('maxtemp_cal', 'min_press')
        self.runTimesTest(req)

    def testGetAvailableTimesWithLocationNamesForPeriod(self):
        """
        Test retrieval of times for a climo table that uses 
        period_start and period_end columns to build DataTimes.
        """
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_mon_season_yr')
        req.setLocationNames('KOMA', 'KABR', 'KDMO')
        req.setParameters('max_temp', 'precip_total')
        self.runTimesTest(req)

    def testGetAvailableTimesWithLocationNamesForDate(self):
        """
        Test retrieval of times for a climo table that uses a date 
        column to build DataTimes.
        """
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.daily_climate')
        req.setLocationNames('KOMA', 'KABR', 'KDMO')
        req.setParameters('max_temp', 'precip', 'avg_wind_speed')
        self.runTimesTest(req)

    def testGetAvailableTimesWithConstraint(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_asos_monthly')
        req.addIdentifier('maxtemp_mon', RequestConstraint.new('<', 75))
        req.setParameters('maxtemp_mon', 'min_sea_press')
        self.runTimesTest(req)

    def testGetGeometryData(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_asos_monthly')
        req.setLocationNames('KFNB')
        req.setParameters('maxtemp_mon', 'min_sea_press')
        self.runGeometryDataTest(req)

    def testGetGeometryDataForYearAndDayOfYearTable(self):
        """
        Test retrieval of data for a climo table that uses year and 
        day_of_year columns to build DataTimes.
        """
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_asos_daily')
        req.setLocationNames('KFNB')
        req.setParameters('maxtemp_cal', 'min_press')
        self.runGeometryDataTest(req)

    def testGetGeometryDataForPeriodTable(self):
        """
        Test retrieval of data for a climo table that uses a period_start and 
        period_end columns to build DataTimes.
        """
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_mon_season_yr')
        req.setLocationNames('KFNB')
        req.setParameters('max_temp', 'precip_total')
        self.runGeometryDataTest(req)

    def testGetGeometryDataForDateTable(self):
        """
        Test retrieval of data for a climo table that uses a date column to 
        build DataTimes.
        """
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.daily_climate')
        req.setLocationNames('KFNB')
        req.setParameters('max_temp', 'precip', 'avg_wind_speed')
        self.runGeometryDataTest(req)

    def testGetGeometryDataWithShortParameter(self):
        """
        Test that a parameter that is stored in Java as a Short is correctly
        retrieved as a number.
        """
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'cli_asos_monthly')
        req.setParameters('month')
        geometryData = self.runGeometryDataTest(req)
        for record in geometryData:
            self.assertIsNotNone(record.getNumber('month'))

    def testGetTableIdentifierValues(self):
        self.runGetIdValuesTest(['table'])

    def testGetColumnIdValuesWithTable(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_asos_monthly')
        idValues = DAL.getIdentifierValues(req, 'year')
        self.assertTrue(hasattr(idValues, '__iter__'))

    def testGetColumnIdValuesWithoutTableThrowsException(self):
        req = DAL.newDataRequest(self.datatype)
        with self.assertRaises(ThriftRequestException):
            idValues = DAL.getIdentifierValues(req, 'year')

    @unittest.skip('avoid EDEX error')
    def testGetColumnIdValuesWithNonexistentTableThrowsException(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'nonexistentjunk')
        with self.assertRaises(ThriftRequestException):
            idValues = DAL.getIdentifierValues(req, 'year')

    @unittest.skip('avoid EDEX error')
    def testGetNonexistentColumnIdValuesThrowsException(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_asos_monthly')
        with self.assertRaises(ThriftRequestException):
            idValues = DAL.getIdentifierValues(req, 'nonexistentjunk')

    def testGetInvalidIdentifierValuesThrowsException(self):
        self.runInvalidIdValuesTest()

    def testGetNonexistentIdentifierValuesThrowsException(self):
        self.runNonexistentIdValuesTest()

    def _runConstraintTest(self, key, operator, value):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'cli_asos_monthly')
        constraint = RequestConstraint.new(operator, value)
        req.addIdentifier(key, constraint)
        req.setParameters('station_code', 'avg_daily_max')
        return self.runGeometryDataTest(req)

    def testGetDataWithEqualsString(self):
        geometryData = self._runConstraintTest('station_code', '=', 'KOMA')
        for record in geometryData:
            self.assertEqual(record.getString('station_code'), 'KOMA')

    def testGetDataWithEqualsUnicode(self):
        geometryData = self._runConstraintTest('station_code', '=', u'KOMA')
        for record in geometryData:
            self.assertEqual(record.getString('station_code'), 'KOMA')

    def testGetDataWithEqualsInt(self):
        geometryData = self._runConstraintTest('avg_daily_max', '=', 70)
        for record in geometryData:
            self.assertEqual(record.getNumber('avg_daily_max'), 70)

    def testGetDataWithEqualsLong(self):
        geometryData = self._runConstraintTest('avg_daily_max', '=', 70L)
        for record in geometryData:
            self.assertEqual(record.getNumber('avg_daily_max'), 70)

    def testGetDataWithEqualsFloat(self):
        geometryData = self._runConstraintTest('avg_daily_max', '=', 69.2)
        for record in geometryData:
            self.assertEqual(round(record.getNumber('avg_daily_max'), 1), 69.2)

    def testGetDataWithEqualsNone(self):
        geometryData = self._runConstraintTest('station_code', '=', None)
        self.assertEqual(len(geometryData), 0)

    def testGetDataWithNotEquals(self):
        geometryData = self._runConstraintTest('station_code', '!=', 'KOMA')
        for record in geometryData:
            self.assertNotEqual(record.getString('station_code'), 'KOMA')

    def testGetDataWithNotEqualsNone(self):
        geometryData = self._runConstraintTest('station_code', '!=', None)
        for record in geometryData:
            self.assertNotEqual(record.getType('station_code'), 'NULL')

    def testGetDataWithGreaterThan(self):
        geometryData = self._runConstraintTest('avg_daily_max', '>', 70)
        for record in geometryData:
            self.assertGreater(record.getNumber('avg_daily_max'), 70)

    def testGetDataWithLessThan(self):
        geometryData = self._runConstraintTest('avg_daily_max', '<', 70)
        for record in geometryData:
            self.assertLess(record.getNumber('avg_daily_max'), 70)

    def testGetDataWithGreaterThanEquals(self):
        geometryData = self._runConstraintTest('avg_daily_max', '>=', 70)
        for record in geometryData:
            self.assertGreaterEqual(record.getNumber('avg_daily_max'), 70)

    def testGetDataWithLessThanEquals(self):
        geometryData = self._runConstraintTest('avg_daily_max', '<=', 70)
        for record in geometryData:
            self.assertLessEqual(record.getNumber('avg_daily_max'), 70)

    def testGetDataWithInTuple(self):
        collection = ('KOMA', 'KABR')
        geometryData = self._runConstraintTest('station_code', 'in', collection)
        for record in geometryData:
            self.assertIn(record.getString('station_code'), collection)

    def testGetDataWithInList(self):
        collection = ['KOMA', 'KABR']
        geometryData = self._runConstraintTest('station_code', 'in', collection)
        for record in geometryData:
            self.assertIn(record.getString('station_code'), collection)

    def testGetDataWithInGenerator(self):
        collection = ('KOMA', 'KABR')
        generator = (item for item in collection)
        geometryData = self._runConstraintTest('station_code', 'in', generator)
        for record in geometryData:
            self.assertIn(record.getString('station_code'), collection)

    def testGetDataWithInvalidConstraintTypeThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('station_code', 'junk', 'KOMA')

    def testGetDataWithInvalidConstraintValueThrowsException(self):
        with self.assertRaises(TypeError):
            self._runConstraintTest('station_code', '=', {})

    def testGetDataWithEmptyInConstraintThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('station_code', 'in', [])

    def testGetDataWithTimeRangeWithYearAndMonth1(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_asos_monthly')
        req.setLocationNames('KFNB')
        req.setParameters('maxtemp_mon', 'min_sea_press')
        startTime = datetime.datetime(2009, 1, 1)
        endTime = datetime.datetime(2009, 12, 31)
        tr = TimeRange(startTime, endTime)
        self.runGeometryDataTestWithTimeRange(req, tr)

    def testGetDataWithTimeRangeWithYearAndMonth2(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_asos_monthly')
        req.setLocationNames('KFNB')
        req.setParameters('maxtemp_mon', 'min_sea_press')
        startTime = datetime.datetime(2008, 1, 1)
        endTime = datetime.datetime(2009, 3, 31)
        tr = TimeRange(startTime, endTime)
        self.runGeometryDataTestWithTimeRange(req, tr)

    def testGetDataWithTimeRangeWithYearAndMonth3(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_asos_monthly')
        req.setLocationNames('KFNB')
        req.setParameters('maxtemp_mon', 'min_sea_press')
        startTime = datetime.datetime(2007, 7, 1)
        endTime = datetime.datetime(2009, 3, 31)
        tr = TimeRange(startTime, endTime)
        self.runGeometryDataTestWithTimeRange(req, tr)

    def testGetDataWithTimeRangeWithYearAndDayOfYear1(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_asos_daily')
        req.setLocationNames('KFNB')
        req.setParameters('maxtemp_cal', 'min_press')
        startTime = datetime.datetime(2009, 1, 1)
        endTime = datetime.datetime(2009, 7, 31)
        tr = TimeRange(startTime, endTime)
        self.runGeometryDataTestWithTimeRange(req, tr)

    def testGetDataWithTimeRangeWithYearAndDayOfYear2(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_asos_daily')
        req.setLocationNames('KFNB')
        req.setParameters('maxtemp_cal', 'min_press')
        startTime = datetime.datetime(2008, 7, 1)
        endTime = datetime.datetime(2009, 3, 31)
        tr = TimeRange(startTime, endTime)
        self.runGeometryDataTestWithTimeRange(req, tr)

    def testGetDataWithTimeRangeWithYearAndDayOfYear3(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_asos_daily')
        req.setLocationNames('KFNB')
        req.setParameters('maxtemp_cal', 'min_press')
        startTime = datetime.datetime(2007, 7, 1)
        endTime = datetime.datetime(2009, 3, 31)
        tr = TimeRange(startTime, endTime)
        self.runGeometryDataTestWithTimeRange(req, tr)

    def testGetDataWithTimeRangeWithPeriodTable(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.cli_mon_season_yr')
        req.setLocationNames('KFNB')
        req.setParameters('max_temp', 'precip_total')
        startTime = datetime.datetime(2007, 7, 1)
        endTime = datetime.datetime(2009, 3, 31)
        tr = TimeRange(startTime, endTime)
        self.runGeometryDataTestWithTimeRange(req, tr)

    def testGetDataWithTimeRangeWithForDateTable(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.daily_climate')
        req.setLocationNames('KFNB')
        req.setParameters('max_temp', 'precip', 'avg_wind_speed')
        startTime = datetime.datetime(2007, 7, 1)
        endTime = datetime.datetime(2009, 3, 31)
        tr = TimeRange(startTime, endTime)
        self.runGeometryDataTestWithTimeRange(req, tr)

