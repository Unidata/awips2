/*
 * Created on Aug 6, 2003
 * Chip Gobs
 */
package ohd.hseb.db;

import java.sql.*;
import java.util.*;
import java.util.Date;

/**
 * This class is the abstract super class of all generated subclasses. It
 * provides some common methods that do not need to be implemented by each
 * generated class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------------------------
 * May 13, 2011  8079       jnjanga    added method executeQuery(Statement,String)
 *                                     for running arbitrary SQL queries.
 * 
 * </pre>
 * 
 * @author Chip Gobs
 */
public abstract class DbTable {
    private String _tableName;

    private Database _database;

    public abstract List select(String where) throws SQLException;

    protected DbTable(Database database) {
        _database = database;
    }

    protected Database getDatabase() {
        return _database;
    }

    protected Statement getStatement() {
        return _database.getStatement();
    }

    protected Connection getConnection() {
        return _database.getConnection();
    }

    protected void setDatabase(Database database) {
        _database = database;
    }

    public String getTableName() {
        return _tableName;
    }

    public void setTableName(String tableName) {
        _tableName = tableName;
    }

    // *****************************************************************************
    // printRecordList() - this method is called with a List of CurPrecipRecord
    // objects and prints them to standard out
    // *****************************************************************************

    public void printRecordList(List recordList) {
        DbRecord record = null;

        for (int ctr = 0; ctr < recordList.size(); ctr++) {
            record = (DbRecord) recordList.get(ctr);
            System.out.println(record.toString());
        }

        System.out.println(recordList.size()
                + " record(s) returned from printRecord. ");

    } // end of printRecordList method

    // ---------------------------------------------------------
    public String getStringFromDate(java.sql.Date date) {
        return DbTimeHelper.getStringFromDate(date);
    }

    // -------------------------------------------------------
    public long getLongTimeFromDateTimeString(String timeString) {
        return DbTimeHelper.getLongTimeFromDateTimeString(timeString);
    }

    // ---------------------------------------------------------
    public long getLongTimeFromDateString(String timeString) {
        return DbTimeHelper.getLongTimeFromDateString(timeString);
    }

    // --------------------------------------------------------
    public long getLongTimeFromTimeToSecondsString(String timeString) {
        return DbTimeHelper.getLongTimeFromTimeToSecondsString(timeString);
    }

    // ---------------------------------------------------------

    public String getDateTimeStringFromLongTime(long time) {
        return DbTimeHelper.getDateTimeStringFromLongTime(time);
    }

    // ---------------------------------------------------------
    public String getDateStringFromLongTime(long time) {
        return DbTimeHelper.getDateStringFromLongTime(time);
    }

    // ---------------------------------------------------------

    public String getStringFromTimestamp(Timestamp timestamp) {
        return DbTimeHelper.getStringFromTimestamp(timestamp);
    }

    // ---------------------------------------------------------

    public String getTrimmedString(String string) {
        if (string != null) {
            string = string.trim();
        }

        return string;
    }

    // *****************************************************************************
    // getTimeStampFromString() is used to convert DateTime year to sec
    // variables
    // It is typically called from a concrete generated class to
    // get the time in the database in UTC - which is to say, the time the
    // database
    // already stores it as.
    // *****************************************************************************
    public Date getDateTimeFromString(String timeString) {
        return DbTimeHelper.getDateTimeFromString(timeString);
    }

    // *****************************************************************************
    // getDateFromString() is used to convert Date year to day variables
    // It is typically called from a concrete generated class to
    // get the time in the database in UTC - which is to say, the time the
    // database
    // already stores it as.
    // *****************************************************************************
    public Date getDateFromString(String timeString) {
        return DbTimeHelper.getDateFromString(timeString);
    }

    // *****************************************************************************
    // selectCount returns the count of records in the database
    // that meet the select criteria in the where clause.
    // *****************************************************************************

    public int selectCount(String where) throws SQLException {
        int recordCount = -1;

        // Create the SQL statement and issue it

        // construct the select statment
        String selectStatement = "SELECT count(*) FROM " + getTableName() + " "
                + where;

        // get the result set back from the query to the database
        ResultSet rs = _database.getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next()) {
            // store the recordCount
            recordCount = rs.getInt(1);
        }
        // Close the result set
        rs.close();

        // return a List which holds the CurPrecipRecord objects
        return recordCount;

    } // end of selectCount method

    // -------------------------------------------------------------------------------------------

    public String getString(ResultSet rs, int index) throws SQLException {
        String value = getTrimmedString(rs.getString(index));

        return value;
    }

    // -------------------------------------------------------------------------------------------

    public long getDate(ResultSet rs, int index) throws SQLException {
        long date = getNullDate();
        String stringValue = rs.getString(index);

        if (!rs.wasNull()) {
            date = getLongTimeFromDateString(stringValue);
        } else {
            date = getNullDate();
        }

        return date;
    }

    // -------------------------------------------------------------------------------------------

    public long getTimeStamp(ResultSet rs, int index) throws SQLException {
        long datetime = getNullTimeStamp();
        String value = rs.getString(index);

        if (!rs.wasNull()) {
            datetime = getLongTimeFromDateTimeString(value);
        }

        return datetime;
    }

    // -------------------------------------------------------------------------------------------

    public long getTime(ResultSet rs, int index) throws SQLException {
        long time = getNullTime();
        String value = rs.getString(index);

        if (!rs.wasNull()) {
            time = getLongTimeFromTimeToSecondsString(value);
        }

        return time;
    }

    // -------------------------------------------------------------------------------------------

    public double getDouble(ResultSet rs, int index) throws SQLException {
        double value = rs.getDouble(index);

        if (rs.wasNull()) {
            value = getNullDouble();
        }

        return value;
    }

    // -------------------------------------------------------------------------------------------

    public float getFloat(ResultSet rs, int index) throws SQLException {
        float value = rs.getFloat(index);

        if (rs.wasNull()) {
            value = getNullFloat();
        }

        return value;
    }

    // -------------------------------------------------------------------------------------------

    public float getReal(ResultSet rs, int index) throws SQLException {
        float value = rs.getFloat(index);

        if (rs.wasNull()) {
            value = getNullReal();
        }

        return value;
    }

    // -------------------------------------------------------------------------------------------

    public int getInt(ResultSet rs, int index) throws SQLException {
        int value = rs.getInt(index);

        if (rs.wasNull()) {
            value = getNullInt();
        }

        return value;
    }

    // -------------------------------------------------------------------------------------------
    public long getLong(ResultSet rs, int index) throws SQLException {
        long value = rs.getLong(index);

        if (rs.wasNull()) {
            value = getNullLong();
        }

        return value;
    }

    // -------------------------------------------------------------------------------------------
    public short getShort(ResultSet rs, int index) throws SQLException {
        short value = rs.getShort(index);

        if (rs.wasNull()) {
            value = getNullShort();
        }

        return value;
    }

    // -------------------------------------------------------------------------------------------

    public byte[] getBytes(ResultSet rs, int index) throws SQLException {
        byte[] value = rs.getBytes(index);

        if (rs.wasNull()) {
            value = getNullBytes();
        }

        return value;
    }

    // -------------------------------------------------------------------------------------------
    public void setDateOld(PreparedStatement statement, int index, long value)
            throws SQLException {
        if (isNull(value)) {
            statement.setNull(index, Types.DATE);
        } else {
            statement.setString(index, getDateStringFromLongTime(value));
        }

        return;
    }

    public void setDate(PreparedStatement statement, int index, long value)
            throws SQLException {
        if (isNull(value)) {
            statement.setNull(index, Types.DATE);
        } else {
            TimeZone tz = TimeZone.getTimeZone("UTC");
            Calendar cal = Calendar.getInstance(tz);
            cal.setTimeInMillis(value);
            statement.setDate(index, new java.sql.Date(value), cal);
        }

        return;
    }

    // -------------------------------------------------------------------------------------------

    public void setTimeStamp(PreparedStatement statement, int index, long value)
            throws SQLException {

        // passes unit test
        // setTimeStampByString(statement, index, value);

        // passes unit test
        // setTimeStampByValueWithCalendar(statement, index, value);

        // passes unit test
        setTimeStampByValueWithCalendarButWithoutSettingCalendarTime(statement,
                index, value);

        // Bad one below fails JUnit test

        // setTimeStampByValueWithoutCalendar(statement, index, value);
    }

    private void setTimeStampByString(PreparedStatement statement, int index,
            long value) throws SQLException {
        String header = "DbTable.setTimeStampByString(): ";
        if (isNull(value)) {
            statement.setNull(index, Types.TIMESTAMP);
        } else {
            String dateTimeString = DbTimeHelper
                    .getDateTimeStringFromLongTime(value);
            Timestamp timestamp = Timestamp.valueOf(dateTimeString);

            System.out.println(header + "timestamp = " + timestamp);
            statement.setTimestamp(index, timestamp);
        }

        return;
    }

    private void setTimeStampByValueWithoutCalendar(
            PreparedStatement statement, int index, long value)
            throws SQLException {
        String header = "DbTable.setTimeStampByValueWithoutCalendar(): ";
        if (isNull(value)) {
            statement.setNull(index, Types.TIMESTAMP);
        } else {
            Timestamp timestamp = new Timestamp(value);
            System.out.println(header + "timestamp = " + timestamp);
            statement.setTimestamp(index, timestamp);
        }

        return;
    }

    private void setTimeStampByValueWithCalendar(PreparedStatement statement,
            int index, long value) throws SQLException {
        String header = "DbTable.setTimeStampByValueWithCalendar(): ";
        if (isNull(value)) {
            statement.setNull(index, Types.TIMESTAMP);
        } else {
            TimeZone tz = TimeZone.getTimeZone("UTC");
            Calendar cal = Calendar.getInstance(tz);
            cal.setTimeInMillis(value);
            Timestamp timestamp = new Timestamp(value);

            // System.out.println(header + "timestamp = " + timestamp);
            statement.setTimestamp(index, timestamp, cal);

        }

        return;
    }

    private void setTimeStampByValueWithCalendarButWithoutSettingCalendarTime(
            PreparedStatement statement, int index, long value)
            throws SQLException {
        String header = "DbTable.setTimeStampByValueWithCalendarButWithoutSettingCalendarTime(): ";
        if (isNull(value)) {
            statement.setNull(index, Types.TIMESTAMP);
        } else {
            TimeZone tz = TimeZone.getTimeZone("UTC");
            Calendar cal = Calendar.getInstance(tz);
            Timestamp timestamp = new Timestamp(value);

            // System.out.println(header + "timestamp = " + timestamp);
            statement.setTimestamp(index, timestamp, cal);

        }

        return;
    }

    // -------------------------------------------------------------------------------------------

    public void setTime(PreparedStatement statement, int index, long value)
            throws SQLException {
        if (isNull(value)) {
            statement.setNull(index, Types.TIME);
        } else {
            TimeZone tz = TimeZone.getTimeZone("UTC");
            Calendar cal = Calendar.getInstance(tz);

            cal.setTimeInMillis(value);

            java.sql.Time time = new java.sql.Time(value);

            statement.setTime(index, time, cal);
        }

        return;
    }

    // -------------------------------------------------------------------------------------------

    public void setString(PreparedStatement statement, int index, String value)
            throws SQLException {
        statement.setString(index, value);

        return;
    }

    // -------------------------------------------------------------------------------------------

    public void setArray(PreparedStatement statement, int index, String value)
            throws SQLException {
        // statement.setString(index, value);

        DbArrayColumn arrayColumn = new DbArrayColumn(stripSingleQuotes(value));

        statement.setArray(index, arrayColumn);
        return;
    }

    private String stripSingleQuotes(String origString) {
        int length = origString.length();
        int indexNotToInclude = length - 1;

        String newString = origString.substring(1, indexNotToInclude);

        return newString;
    }

    // -------------------------------------------------------------------------------------------

    public void setBytes(PreparedStatement statement, int index, byte[] value)
            throws SQLException {
        if (isNull(value)) {
            statement.setNull(index, Types.LONGVARBINARY);
        } else {
            statement.setBytes(index, value);
        }

        return;
    }

    // -------------------------------------------------------------------------------------------

    public void setInt(PreparedStatement statement, int index, int value)
            throws SQLException {
        if (isNull(value)) {
            statement.setNull(index, Types.INTEGER);
        } else {
            statement.setInt(index, value);
        }

        return;
    }

    // -------------------------------------------------------------------------------------------
    public void setLong(PreparedStatement statement, int index, long value)
            throws SQLException {
        if (isNull(value)) {
            statement.setNull(index, Types.BIGINT);
        } else {
            statement.setLong(index, value);
        }
        return;
    }

    // -------------------------------------------------------------------------------------------
    public void setShort(PreparedStatement statement, int index, short value)
            throws SQLException {
        if (isNull(value)) {
            statement.setNull(index, Types.SMALLINT);
        } else {
            statement.setShort(index, value);
        }
        return;
    }

    // -------------------------------------------------------------------------------------------
    public void setDouble(PreparedStatement statement, int index, double value)
            throws SQLException {
        if (isNull(value)) {
            statement.setNull(index, Types.DOUBLE);
        } else {
            statement.setDouble(index, value);
        }

        return;
    }

    // -------------------------------------------------------------------------------------------
    public void setFloat(PreparedStatement statement, int index, float value)
            throws SQLException {
        if (isNull(value)) {
            statement.setNull(index, Types.FLOAT);
        } else {
            statement.setFloat(index, value);
        }

        return;
    }

    // -------------------------------------------------------------------------------------------
    public void setReal(PreparedStatement statement, int index, float value)
            throws SQLException {
        if (isNull(value)) {
            statement.setNull(index, Types.REAL);
        } else {
            statement.setFloat(index, value); // intentionally set to setFloat,
                                              // since setReal does not exist
        }

        return;
    }

    // -------------------------------------------------------------------------------------------

    public static boolean isNull(String value) {
        boolean result = false;

        if (value == null) {
            result = true;
        }

        return result;
    }

    // -------------------------------------------------------------------------------------------

    public static boolean isNull(byte[] value) {
        boolean result = false;

        if (value == null) {
            result = true;
        }

        return result;
    }

    // -------------------------------------------------------------------------------------------
    public static boolean isNull(long value) {
        boolean result = false;

        if (value == getNullLong()) {
            result = true;
        }

        return result;
    }

    // -------------------------------------------------------------------------------------------
    public static boolean isNull(int value) {
        boolean result = false;

        if (value == getNullInt()) {
            result = true;
        }

        return result;
    }

    // -------------------------------------------------------------------------------------------

    public static boolean isNull(short value) {
        boolean result = false;

        if (value == getNullShort()) {
            result = true;
        }

        return result;
    }

    // -------------------------------------------------------------------------------------------

    public static boolean isNull(double value) {
        boolean result = false;

        if (value == getNullDouble()) {
            result = true;
        }

        return result;
    }

    // -------------------------------------------------------------------------------------------

    public static boolean isNull(float value) {
        boolean result = false;

        if (value == getNullFloat()) {
            result = true;
        }

        return result;
    }

    // -------------------------------------------------------------------------------------------

    // -------------------------------------------------------------------------------------------

    public static int getNullInt() {
        return Integer.MIN_VALUE;
    }

    // -------------------------------------------------------------------------------------------

    public static long getNullLong() {
        return Long.MIN_VALUE;
    }

    // -------------------------------------------------------------------------------------------
    public static short getNullShort() {
        return Short.MIN_VALUE;
    }

    // -------------------------------------------------------------------------------------------

    public static float getNullFloat() {
        return -Float.MAX_VALUE;
    }

    // -------------------------------------------------------------------------------------------

    public static float getNullReal() {
        return -Float.MAX_VALUE; // real maps to float
    }

    // -------------------------------------------------------------------------------------------

    public static double getNullDouble() {
        return -Double.MAX_VALUE;
    }

    // -------------------------------------------------------------------------------------------

    public static String getNullString() {
        return null;
    }

    // -------------------------------------------------------------------------------------------

    public static long getNullDate() {
        return Long.MIN_VALUE;
    }

    // -------------------------------------------------------------------------------------------
    public static long getNullTimeStamp() {
        return Long.MIN_VALUE;
    }

    // -------------------------------------------------------------------------------------------

    public static long getNullTime() {
        return Long.MIN_VALUE;
    }

    // -------------------------------------------------------------------------------------------
    public static byte[] getNullBytes() {
        return null;

    }

    public ResultSet executeQuery(Statement stmt, String sql)
            throws SQLException {
        return stmt.executeQuery(sql);
    }

}