/**
 * 
 */
package gov.noaa.nws.ncep.common.dataplugin.ncscat;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

/**
 * NcscatMode - Enum class to centralize and encapsulate all the things that
 * vary among different satellite and data feed types.
 * 
 * //TODO: Consider moving this information entirely to the bundle and/or
 * preferences (.xml/.prm) files, so the Java code can be completely agnostic
 * about satellite data types; would allow extended 'configurability', at the
 * expense of slightly longer bundle/preference files...
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02 Jun 2010  235B       B. Hebbard  Initial creation.
 * 03 Feb 2011  235E       B. Hebbard  Add support for ambiguity variants.
 * 16 Aug 2012             B. Hebbard  Add OSCAT / OSCAT_HI
 * 11 Apr 2014  1128       B. Hebbard  Add longitudeCoding field; change wind sense from boolean to enum.
 * 21 Oct 2014  R4865      B. Hebbard  (TTR 984) Add file-header length and reportType fields to enable refactor to make decoding more robust (and thereby prevent date and other corruption)
 * 
 * </pre>
 * 
 * @author bhebbard
 * @version 1.0
 */

public enum NcscatMode {

    // The only ordering constraint here is that if A.pointsPerRow divides
    // B.pointsPerRow and A and B have the same byteOrder, then A should come
    // before B. (Currently, no such cases exist.) This is so that we can check
    // an unknown data file for 'consistency' with these types, in order, and
    // choose the first one that matches. (In the hypothetical case above, A and
    // B could both pass consistency checking, but we'd want to choose A.)

    // @formatter:off     ppr hdr
        QUIKSCAT        (  76,  0, WindDirectionSense.METEOROLOGICAL, LongitudeCoding.UNSIGNED, ByteOrder.BIG_ENDIAN,    "quikscat" ),
        QUIKSCAT_HI     ( 152,  0, WindDirectionSense.METEOROLOGICAL, LongitudeCoding.UNSIGNED, ByteOrder.BIG_ENDIAN,    "quikscat-hi"),
        ASCAT           (  42,  0, WindDirectionSense.OCEANOGRAPHIC,  LongitudeCoding.UNSIGNED, ByteOrder.BIG_ENDIAN,    "ascat"),
        ASCAT_HI        (  82,  0, WindDirectionSense.OCEANOGRAPHIC,  LongitudeCoding.UNSIGNED, ByteOrder.BIG_ENDIAN,    "ascat-hi"),
        EXASCT          (  42,  0, WindDirectionSense.OCEANOGRAPHIC,  LongitudeCoding.UNSIGNED, ByteOrder.LITTLE_ENDIAN, "Exasct"),
        EXASCT_HI       (  82,  0, WindDirectionSense.OCEANOGRAPHIC,  LongitudeCoding.UNSIGNED, ByteOrder.LITTLE_ENDIAN, "Exasct-hi"),
        OSCAT           (  36,  0, WindDirectionSense.METEOROLOGICAL, LongitudeCoding.UNSIGNED, ByteOrder.BIG_ENDIAN,    "oscat"),
        OSCAT_HI        (  76,  0, WindDirectionSense.METEOROLOGICAL, LongitudeCoding.UNSIGNED, ByteOrder.LITTLE_ENDIAN, "oscat-hi"),
        WSCAT           (  79, 56, WindDirectionSense.METEOROLOGICAL, LongitudeCoding.SIGNED,   ByteOrder.LITTLE_ENDIAN, "wscat"),
        UNKNOWN         (  76,  0, WindDirectionSense.METEOROLOGICAL, LongitudeCoding.UNSIGNED, ByteOrder.BIG_ENDIAN,    "unknown");
    // @formatter:on

    private int pointsPerRow; // number of Wind Vector Cell in each scan row
                              // across satellite track

    private WindDirectionSense windDirectionSense; // is the numeric wind
                                                   // direction the "from"
                                                   // (METEOROLOGICAL)
                                                   // direction, or the "to"
                                                   // (OCEANOGRAPHIC) direction?

    private LongitudeCoding longitudeCoding; // is the two-byte value a SIGNED
                                             // (-18000 -- +18000) or UNSIGNED
                                             // (0 -- 36000) representation of
                                             // the (scaled by 100) longitude
                                             // east of Greenwich?

    private ByteOrder byteOrder; // endianess of data in the byte stream

    private String reportType; // string to use in reportType field of DB

    private int valuesPerPoint = 9; // number of (short int) data values per
                                    // point

    private int bytesPerValue = 2; // all are short int

    private int fileHeaderLength; // length in bytes of per-file header field,
                                  // if any, in input file, before repeating
                                  // per-row data begins; of interest to decoder

    private int rowHeaderLength = 8; // length in bytes of per-row header field
                                     // (2 bytes each for day, hour, min, sec)

    private int bytesPerRow; // length in bytes of each row, comprising row
                             // header and all data values for all points in
                             // that row

    public int getBytesPerRow() {
        return bytesPerRow;
    }

    // Constructor
    NcscatMode(int pointsPerRow, int fileHeaderLength,
            WindDirectionSense windDirectionSense,
            LongitudeCoding longitudeCoding, ByteOrder byteOrder,
            String reportType) {
        this.pointsPerRow = pointsPerRow;
        this.fileHeaderLength = fileHeaderLength;
        this.windDirectionSense = windDirectionSense;
        this.longitudeCoding = longitudeCoding;
        this.byteOrder = byteOrder;
        this.reportType = reportType;

        bytesPerRow = rowHeaderLength + pointsPerRow * valuesPerPoint
                * bytesPerValue;
    }

    public String getReportType() {
        return reportType;
    }

    public int getPointsPerRow() {
        return pointsPerRow;
    }

    public int getFileHeaderLength() {
        return fileHeaderLength;
    }

    public WindDirectionSense getWindDirectionSense() {
        return windDirectionSense;
    }

    public LongitudeCoding getLongitudeCoding() {
        return longitudeCoding;
    }

    public ByteOrder getByteOrder() {
        return byteOrder;
    }

    public enum WindDirectionSense { // numeric direction value gives...
        METEOROLOGICAL, // degrees FROM which wind is blowing
        OCEANOGRAPHIC // degrees TO which wind is blowing
    }

    public enum LongitudeCoding { // 2-byte wvc_lon (Wind Vector Cell -
                                  // longitude) field is (x0.01 deg)...
        SIGNED, // SIGNED short (-18000..+18000)
        UNSIGNED // UNSIGNED short (0..36000 east of Greenwich)
    }

    public static NcscatMode stringToMode(String name) {
        // Given a string, return the corresponding enum
        NcscatMode returnValue = null;
        name = name.toUpperCase();
        name = name.replaceAll("-", "_");
        // TODO: Remove ambiguity number??
        try {
            returnValue = valueOf(name);
        } catch (IllegalArgumentException e) {
            // TODO: Signal unrecognized Ncscat mode string
            returnValue = UNKNOWN;
        }
        return returnValue;
    }

    public boolean consistentWith(ByteBuffer byteBuffer) {
        // Given a ByteBuffer containing ingested binary data of unknown
        // satellite type (mode), determine whether data are consistent
        // with "this" mode. We do this by seeing if date/hour fields
        // repeat where they would be expected to appear, and contain
        // reasonable values for those fields.

        // TODO: Consider moving this to decoder...? (Kind of prefer
        // encapsulating it here, but also like enums to be rather
        // minimalistic...? -bh)

        byteBuffer.order(byteOrder);
        int base = fileHeaderLength;
        for (int cycle = 0; cycle < 2; cycle++) {

            // DAY (of year) candidate fields of consecutive rows out of
            // range...
            short thisCandidateDayOfYear = byteBuffer.getShort(base);
            short nextCandidateDayOfYear = byteBuffer.getShort(base
                    + bytesPerRow);
            if (thisCandidateDayOfYear < 1 || thisCandidateDayOfYear > 366
                    || nextCandidateDayOfYear < 1
                    || nextCandidateDayOfYear > 366)
                // ...means (right away) data can't be this type
                return false;
            // ...but if they don't match...
            if (thisCandidateDayOfYear != nextCandidateDayOfYear) {
                // ...can't rule it out (since first two rows could straddle a
                // day boundary)...but can skip checking hour and go to check
                // next pair of rows...
                break;
            }

            // HOUR candidate fields (of consecutive rows) in range...?
            short thisCandidateHour = byteBuffer.getShort(base + 2);
            short nextCandidateHour = byteBuffer.getShort(base + 2
                    + bytesPerRow);
            if (thisCandidateHour < 0 || thisCandidateHour > 23
                    || nextCandidateHour < 0 || nextCandidateHour > 23)
                return false;
            // ...and if they do match (as well as day field above), then we
            // conclude consistency
            if (byteBuffer.getShort(base) == byteBuffer.getShort(base
                    + bytesPerRow)) {
                return true;
            }
            // ...but if they don't, again, first two rows could straddle an
            // hour boundary, so go to next pair of rows (cycle)...
            base += bytesPerRow;
        }
        // We've made it through 2 consecutive cycles, and neither (1st/2nd
        // nor 2nd/3rd) day+hour match, so conclude (assuming real consecutive
        // rows are not separated by an hour or more) data not consistent with
        // this type
        return false;
    }

}
