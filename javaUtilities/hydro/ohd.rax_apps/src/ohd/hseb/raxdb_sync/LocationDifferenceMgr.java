package ohd.hseb.raxdb_sync;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.db.DbRecord;
import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.ihfsdb.generated.CountiesRecord;
import ohd.hseb.ihfsdb.generated.LocationRecord;
import ohd.hseb.raxdb.generated.RaxLocationRecord;
import ohd.hseb.raxdb.generated.RaxLocationTable;


public class LocationDifferenceMgr extends BaseDifferenceMgr implements IDifferenceMgr
{

    private Map _ihfsCountyfipsMap;

    public LocationDifferenceMgr(RaxSyncDataMgr dm)
    {
        setDataMgr(dm);
        
        setupOutputFiles("Location");

    } // end of Constructor
    
    //----------------------------------------------------------------------------------------------
    public DiffSet findDifferences()
    {
        DiffSet tableDifferences = new DiffSet();
        
        List ihfsRecordList = null;
        List raxRecordList = null;
        
        RecordDifference oneDifference = null;

        logApplicationMessage("\n" + _dm.getDateTimeStamp() + " Begin Analyzing Location Table records...");
        
        // create a Map of county|state to countyfips number
        createCountyfipsMap();
        
        ihfsRecordList = _dm.getIhfsLocationRecordList();
        _numberOfIhfsRecords = _numberOfIhfsRecords + ihfsRecordList.size();
        raxRecordList = _dm.getRaxLocationRecordList();

        //add the RAX records to a Map
        Map raxLidMap = new HashMap();
        for (int i=0; i < raxRecordList.size(); i++)
        {
            RaxLocationRecord record = (RaxLocationRecord) raxRecordList.get(i);
            // NOTE: if there is more than one RAX record with the same lid,
            // the map entry is overwritten and the last one in (which is the later sbd) wins
            raxLidMap.put(record.getLid(), record);
        } // end of raxRecordList for loop
        
        // loop through IHFS record list and check if same lid found in RAX
        // if not, this needs to be added as new to RAX
        // if same lid found then check to see if they are "different"
        for (int i=0; i<ihfsRecordList.size(); i++)
        {
            List fieldDifferenceList = null;

            LocationRecord currentIhfsRecord = (LocationRecord) ihfsRecordList.get(i);
            String currentIhfsLid = currentIhfsRecord.getLid();
            RaxLocationRecord raxRecord = (RaxLocationRecord) raxLidMap.get(currentIhfsLid);

            if (raxRecord == null)  //not in the rax database
            {
                oneDifference = createDifferenceEntry(RecordDifferenceOriginType.NEW, currentIhfsRecord, null, null);
                tableDifferences.addDifference(oneDifference);
                _numberOfNewRecords++;
            }
            else //match found
            {
                fieldDifferenceList = getFieldDifferenceList(currentIhfsRecord, raxRecord);
                if (fieldDifferenceList.size() == 0) //same
                {
                    _numberOfSameRecords++;
                }
                else //different
                {
                    oneDifference = createDifferenceEntry(RecordDifferenceOriginType.MOD, currentIhfsRecord, raxRecord, fieldDifferenceList);
                    tableDifferences.addDifference(oneDifference);
                    _numberOfModRecords++;
                } 
            }

        } // end of ihfsRecordList for loop

        displayEndOfFindDifferencesMessage("Location");
        
        return tableDifferences;

    } // end of findDifferences method
    
    //----------------------------------------------------------------------------------------------
    private List getFieldDifferenceList(LocationRecord ihfsRecord, RaxLocationRecord raxRecord)
    {
        List fieldDifferenceList = new ArrayList();

        checkAndAddToFieldDifferenceList("name", makeRaxLocNameFromIhfsRecord(ihfsRecord), raxRecord.getName(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("det", makeMixedCase(ihfsRecord.getDet()), raxRecord.getDet(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("lat", ihfsRecord.getLat(), raxRecord.getLat(), 4, fieldDifferenceList);
        checkAndAddToFieldDifferenceList("lon", ConvertRaxLonFromIhfsLon(ihfsRecord.getLon()), raxRecord.getLon(), 4, fieldDifferenceList);   
        checkAndAddToFieldDifferenceList("elev", makeRaxLocElevFromIhfsRecord(ihfsRecord), raxRecord.getElev(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("state", makeRaxLocStateIdFromIhfsRecord(ihfsRecord), raxRecord.getState(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("hsa", ihfsRecord.getHsa(), raxRecord.getHsa(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("wfo", ihfsRecord.getWfo(), raxRecord.getWfo(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("rfc", makeRaxLocRfcIdFromIhfsRecord(ihfsRecord), raxRecord.getRfc(), fieldDifferenceList);

        return fieldDifferenceList;

    } // end of getFieldDifferenceList method
    
    //----------------------------------------------------------------------------------------------
    private RecordDifference createDifferenceEntry(RecordDifferenceOriginType type, LocationRecord ihfsRecord,
                                            final RaxLocationRecord raxRecord, List fieldDiffList)
    {
        RecordDifference oneDifferenceRecord;
        RaxLocationRecord newRaxRecord = null;
        
        if (type == RecordDifferenceOriginType.NEW)
        {
            newRaxRecord = new RaxLocationRecord();
            
            newRaxRecord.setLid(ihfsRecord.getLid());
            newRaxRecord.setSbd(setRaxLocSbdFromIhfsRecord(ihfsRecord));
            newRaxRecord.setSed(DbTable.getNullDate());    // set to null to show as most current record
            newRaxRecord.setGoes(DbTable.getNullString());  // set to null as per adbinit rules
            newRaxRecord.setName(makeRaxLocNameFromIhfsRecord(ihfsRecord));
            newRaxRecord.setDet(makeMixedCase(ihfsRecord.getDet()));
            newRaxRecord.setLat(ihfsRecord.getLat());
            newRaxRecord.setLon(ConvertRaxLonFromIhfsLon(ihfsRecord.getLon()));
            newRaxRecord.setElev(makeRaxLocElevFromIhfsRecord(ihfsRecord));
            newRaxRecord.setState(makeRaxLocStateIdFromIhfsRecord(ihfsRecord));
            newRaxRecord.setHuc(DbTable.getNullString());  // set to null as per adbinit rules
            newRaxRecord.setCountyfips(makeRaxLocCountyfipsFromIhfsRecord(ihfsRecord));
            newRaxRecord.setZon(DbTable.getNullString());  // set to null as per adbinit rules
            newRaxRecord.setHsa(ihfsRecord.getHsa());
            newRaxRecord.setWfo(ihfsRecord.getWfo());
            newRaxRecord.setPost(setRaxLocPostFromIhfsRecord(ihfsRecord));
            newRaxRecord.setDbsource(DbTable.getNullString());  // set to null as per adbinit rules
            newRaxRecord.setRfc(makeRaxLocRfcIdFromIhfsRecord(ihfsRecord));
            newRaxRecord.setCountryfips(makeRaxLocCountryIdFromIhfsRecord(ihfsRecord));
        }
        else if (type == RecordDifferenceOriginType.MOD)
        {
            newRaxRecord =  new RaxLocationRecord(raxRecord);            
        }
        
        oneDifferenceRecord = new RecordDifference(type, this, ihfsRecord, newRaxRecord, fieldDiffList);
        return oneDifferenceRecord;
        
    } // end of createDifferenceEntry method
    
    //----------------------------------------------------------------------------------------------
    public void reportDifferences(RecordDifference oneRecordDifference)
    {

        FieldDifference oneFieldDifference = new FieldDifference();

        LocationRecord ihfsRecord = (LocationRecord) oneRecordDifference.getIhfsRecord();
        RaxLocationRecord raxRecord = (RaxLocationRecord) oneRecordDifference.getRaxRecord();
        
        if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
        {
            logRecordDifferenceMessage("\nDifferences found in Table = Location" + " for Primary Key = |"+ raxRecord.getLid() +
                                          "|" + displayDate(raxRecord.getSbd()) + "|");
            for (int j=0; j<oneRecordDifference.getFieldDifferenceList().size(); j++)
            {
                oneFieldDifference = (FieldDifference) oneRecordDifference.getFieldDifferenceList().get(j); 
                logRecordDifferenceMessage("Column = " + oneFieldDifference.getName() + "   IHFS_value = "
                            + oneFieldDifference.getIhfsValue() + "   Rax_value = " + oneFieldDifference.getRaxValue());
             }
        }
        else
        {
            
            logNewRecordMessage("\nA new Location record will need to be created in RAX db for Primary Key = |"+ raxRecord.getLid() +
                                "|" + displayDate(raxRecord.getSbd()) + "|" +
                                "\nlid = " + raxRecord.getLid() +
                                "\nsbd = " + displayDate(raxRecord.getSbd()) +
                                "\nsed = " + displayDate(raxRecord.getSed()) +
                                "\ngoes = " + raxRecord.getGoes() +
                                "\nname = " + raxRecord.getName() +
                                "\ndet = " + raxRecord.getDet() +
                                "\nlat = " + displayLatLon(raxRecord.getLat()) +
                                "\nlon = " + displayLatLon(raxRecord.getLon()) +
                                "\nelev = " + displayInt(raxRecord.getElev()) +
                                "\nstate = " + raxRecord.getState() +
                                "\nhuc = " + raxRecord.getHuc() +
                                "\ncountyfips = " + raxRecord.getCountyfips() +
                                "\nzon = " + raxRecord.getZon() +
                                "\nhsa = " + raxRecord.getHsa() +
                                "\npost = " + raxRecord.getPost() +
                                "\ndbsource = " + raxRecord.getDbsource() +
                                "\nrfc = " + raxRecord.getRfc() +
                                "\ncountryfips = " + raxRecord.getCountryfips());

            logDebugMessage("\nA new Location record will need to be created in RAX db for Primary Key = |"+ raxRecord.getLid() +
                            "|" + displayDate(raxRecord.getSbd()) + "|" +
                            "\nusing the following information from the IHFS db:" +
                            "\nlid = " + raxRecord.getLid() + "   IHFS_lid = " + ihfsRecord.getLid() +
                            "\nsbd = " + displayDate(raxRecord.getSbd()) +
                            "   IHFS_sbd = " + displayDate(ihfsRecord.getSbd()) +
                            "   IHFS_lrevise = " + displayDate(ihfsRecord.getLrevise()) +
                            "\nsed = " + displayDate(raxRecord.getSed()) +
                            "\ngoes = " + raxRecord.getGoes() +
                            "\nname = " + raxRecord.getName() + "   IHFS_name = " + ihfsRecord.getName() + "   IHFS_detail = " + ihfsRecord.getDetail() +
                            "\ndet = " + raxRecord.getDet() + "   IHFS_det = " + ihfsRecord.getDet() +
                            "\nlat = " + displayLatLon(raxRecord.getLat()) + "   IHFS_lat = " + displayLatLon(ihfsRecord.getLat()) +
                            "\nlon = " + displayLatLon(raxRecord.getLon()) + "   IHFS_lon = " + displayLatLon(ihfsRecord.getLon()) +
                            "\nelev = " + displayInt(raxRecord.getElev()) + "   IHFS_elev = " + displayDouble(ihfsRecord.getElev()) +
                            "\nstate = " + raxRecord.getState() + "   IHFS_state = " + ihfsRecord.getState() +
                            "\nhuc = " + raxRecord.getHuc() +
                            "\ncountyfips = " + raxRecord.getCountyfips() + "   IHFS_county = " + ihfsRecord.getCounty() + "   IHFS_state = " + ihfsRecord.getState() +
                            "\nzon = " + raxRecord.getZon() +
                            "\nhsa = " + raxRecord.getHsa() + "   IHFS_hsa = " + ihfsRecord.getHsa() +
                            "\npost = " + raxRecord.getPost() + "   IHFS_post = " + ihfsRecord.getPost() +
                            "\ndbsource = " + raxRecord.getDbsource() +
                            "\nrfc = " + raxRecord.getRfc() + "   IHFS_rfc = " + ihfsRecord.getRfc() +
                            "\ncountryfips = " + raxRecord.getCountryfips());
        }
        
    } // end of reportDifferences method

    //----------------------------------------------------------------------------------------------
    public int processDifferences(RecordDifference oneRecordDifference)
    {
        int numberOfFailedDbUpdates = 0;
        RaxLocationTable raxTable = null;
        RaxLocationRecord newRaxRecord = null;

        LocationRecord ihfsRecord = (LocationRecord) oneRecordDifference.getIhfsRecord();
        RaxLocationRecord raxRecord = (RaxLocationRecord) oneRecordDifference.getRaxRecord();

        raxTable = _dm.getRaxLocationTable();

        if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
        {
            newRaxRecord =  new RaxLocationRecord(raxRecord);

            newRaxRecord.setSed(System.currentTimeMillis());

            numberOfFailedDbUpdates = processRaxInsertUpdate(UPDATE, "Location", raxTable, raxRecord, newRaxRecord);

            newRaxRecord.setSbd(System.currentTimeMillis());
            newRaxRecord.setSed(DbTable.getNullDate());
            newRaxRecord.setName(makeRaxLocNameFromIhfsRecord(ihfsRecord));
            newRaxRecord.setDet(makeMixedCase(ihfsRecord.getDet()));
            newRaxRecord.setLat(ihfsRecord.getLat());
            newRaxRecord.setLon(ConvertRaxLonFromIhfsLon(ihfsRecord.getLon()));
            newRaxRecord.setElev(makeRaxLocElevFromIhfsRecord(ihfsRecord));
            newRaxRecord.setState(makeRaxLocStateIdFromIhfsRecord(ihfsRecord));
            newRaxRecord.setHsa(ihfsRecord.getHsa());
            newRaxRecord.setWfo(ihfsRecord.getWfo());
            newRaxRecord.setRfc(makeRaxLocRfcIdFromIhfsRecord(ihfsRecord));

            numberOfFailedDbUpdates = processRaxInsertUpdate(INSERT, "Location", raxTable, newRaxRecord, null);
        }
        else
        {
            numberOfFailedDbUpdates = processRaxInsertUpdate(INSERT, "Location", raxTable, raxRecord, null);
        }

        return numberOfFailedDbUpdates;
        
    } // end of processDifferences method

    //----------------------------------------------------------------------------------------------
    private void createCountyfipsMap()
    {
        List ihfsCountiesRecordList = null;
        
        ihfsCountiesRecordList = _dm.getIhfsCountiesRecordList();

        // MAP countyfips code to county|state key
        _ihfsCountyfipsMap = new HashMap();
   
        for (int i=0; i < ihfsCountiesRecordList.size(); i++)
        {
            CountiesRecord record = (CountiesRecord) ihfsCountiesRecordList.get(i);
            String countyfipsMapKey = createKeyForMap(record);
            _ihfsCountyfipsMap.put(countyfipsMapKey, record.getCountynum());
            
        } // end of ihfsCountiesRecordList for loop
               
    }// end of createCountyfipsMap method

    //----------------------------------------------------------------------------------------------
    private String createKeyForMap(CountiesRecord record)
    {
        String key = null;
        
        key = record.getCounty() + "|" + record.getState();
        
        return key;
    }

    //----------------------------------------------------------------------------------------------
    private String createKeyForMap(LocationRecord record)
    {
        String key = null;
        
        key = record.getCounty() + "|" + record.getState();
        
        return key;
    }

    //----------------------------------------------------------------------------------------------
    protected String formatColumnsForInsertUpdateDisplay(DbRecord dbRecord)
    {
        RaxLocationRecord raxRecord = (RaxLocationRecord) dbRecord;

        String formatedString = new String("|" + raxRecord.getLid() + "|"
                + displayDate(raxRecord.getSbd()) + "|"
                + displayDate(raxRecord.getSed()) + "|"
                + raxRecord.getGoes() + "|"
                + raxRecord.getName() + "|"
                + raxRecord.getDet() + "|"
                + displayLatLon(raxRecord.getLat()) + "|"
                + displayLatLon(raxRecord.getLon()) + "|"
                + displayInt(raxRecord.getElev()) + "|"
                + raxRecord.getState() + "|"
                + raxRecord.getHuc() + "|"
                + raxRecord.getCountyfips() + "|"
                + raxRecord.getZon() + "|"
                + raxRecord.getHsa() + "|"
                + raxRecord.getWfo() + "|"
                + displayInt(raxRecord.getPost()) + "|"
                + raxRecord.getDbsource() + "|"
                + raxRecord.getRfc() + "|"
                + raxRecord.getCountryfips() + "|");

        return formatedString;

    } // end formatColumnsForInsertUpdateDisplay method

    //----------------------------------------------------------------------------------------------
    private long setRaxLocSbdFromIhfsRecord(LocationRecord ihfsRecord)
    {
        long raxLocationSbd = 0;
        
        // set RAX sbd to
        // 1) IHFS sbd if not null
        // 2) IHFS lrevise in not null
        // 3) Today's date
        raxLocationSbd = DbTimeHelper.getLongTimeFromDateString("1776-07-04");
        if (! DbTable.isNull(ihfsRecord.getSbd()))
        {
            raxLocationSbd = ihfsRecord.getSbd();
        }
        else if (! DbTable.isNull(ihfsRecord.getLrevise()))
        {
            raxLocationSbd = ihfsRecord.getLrevise();
        }
        else
        {
            raxLocationSbd = System.currentTimeMillis();
        }

        return raxLocationSbd;
        
    }// end of setRaxLocSbdFromIhfsRecord method

    //----------------------------------------------------------------------------------------------
    private String makeRaxLocNameFromIhfsRecord(LocationRecord ihfsRecord)
    {
        String ihfsDetail = null;
        String ihfsLocationName = null;
        String raxLocationName = null;
        
        // to create a RAX location name from an IHFS location record
        // convert the IHFS name to mixed case and concatenate it with a detail that does not contain "AT"
        if (ihfsRecord.getDetail() != null)
        {
            if (! ihfsRecord.getDetail().contains("AT"))
            {
                ihfsDetail = ihfsRecord.getDetail();
            }
        }

        if (ihfsRecord.getName() != null)
        {
            ihfsLocationName = makeMixedCase(ihfsRecord.getName());

            if (ihfsDetail != null)
                raxLocationName = ihfsLocationName + " " + ihfsDetail;
            else
                raxLocationName = ihfsLocationName;
        }

        return raxLocationName;
        
    }// end of makeRaxLocNameFromIhfsRecord method

    //----------------------------------------------------------------------------------------------
    private int makeRaxLocElevFromIhfsRecord(LocationRecord ihfsRecord)
    {
        int raxLocationElev = 0;
        
        // to create a RAX Location elev (Integer) from an IHFS location elev (double)
        // if IHFS elev is null then set to null otherwise add 0.5 (for truncation) and cast as an int
        if (DbTable.isNull(ihfsRecord.getElev()))
        {
            raxLocationElev = DbTable.getNullInt();
        }
        else
            raxLocationElev = (int)(ihfsRecord.getElev() + 0.5);

        return raxLocationElev;
        
    }// end of makeRaxLocElevFromIhfsRecord method
    
    //----------------------------------------------------------------------------------------------
    private String makeRaxLocStateIdFromIhfsRecord(LocationRecord ihfsRecord)
    {
        // set the RAX state ID to the IHFS state ID by default
        String raxLocationStateId = ihfsRecord.getState();
        
        // try to create a RAX state ID from an IHFS location record
        // if the state ID is XX and lid length is 5 charaters or more
        if (ihfsRecord.getState().equalsIgnoreCase("XX") && (ihfsRecord.getLid().length() >= 5))
        {
            if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("A1"))
                raxLocationStateId = "AL";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("A2"))
                raxLocationStateId = "AK";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("A3"))
                raxLocationStateId = "AZ";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("A4"))
                raxLocationStateId = "AR";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("C1"))
                raxLocationStateId = "CA";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("C2"))
                raxLocationStateId = "CO";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("C3"))
                raxLocationStateId = "CT";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("D1"))
                raxLocationStateId = "DE";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("D2"))
                raxLocationStateId = "DC";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("F1"))
                raxLocationStateId = "FL";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("G1"))
                raxLocationStateId = "GA";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("H1"))
                raxLocationStateId = "HI";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("I1"))
                raxLocationStateId = "ID";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("I2"))
                raxLocationStateId = "IL";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("I3"))
                raxLocationStateId = "IN";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("I4"))
                raxLocationStateId = "IA";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("K1"))
                raxLocationStateId = "KS";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("K2"))
                raxLocationStateId = "KY";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("L1"))
                raxLocationStateId = "LA";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("M1"))
                raxLocationStateId = "ME";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("M2"))
                raxLocationStateId = "MD";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("M3"))
                raxLocationStateId = "MA";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("M4"))
                raxLocationStateId = "MI";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("M5"))
                raxLocationStateId = "MN";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("M6"))
                raxLocationStateId = "MS";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("M7"))
                raxLocationStateId = "MO";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("M8"))
                raxLocationStateId = "MT";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("N1"))
                raxLocationStateId = "NE";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("N2"))
                raxLocationStateId = "NV";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("N3"))
                raxLocationStateId = "NH";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("N4"))
                raxLocationStateId = "NJ";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("N5"))
                raxLocationStateId = "NM";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("N6"))
                raxLocationStateId = "NY";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("N7"))
                raxLocationStateId = "NC";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("N8"))
                raxLocationStateId = "ND";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("O1"))
                raxLocationStateId = "OH";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("O2"))
                raxLocationStateId = "OK";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("O3"))
                raxLocationStateId = "OR";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("P1"))
                raxLocationStateId = "PA";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("R1"))
                raxLocationStateId = "RI";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("S1"))
                raxLocationStateId = "SC";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("S2"))
                raxLocationStateId = "SD";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("T1"))
                raxLocationStateId = "TN";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("T2"))
                raxLocationStateId = "TX";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("U1"))
                raxLocationStateId = "UT";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("V1"))
                raxLocationStateId = "VT";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("V2"))
                raxLocationStateId = "VA";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("W1"))
                raxLocationStateId = "WA";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("W2"))
                raxLocationStateId = "WV";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("W3"))
                raxLocationStateId = "WI";
            else if (ihfsRecord.getLid().substring(3,5).equalsIgnoreCase("W4"))
                raxLocationStateId = "WY";
        }

        return raxLocationStateId;
        
    }// end of makeRaxLocStateIdFromIhfsRecord method
    
    //----------------------------------------------------------------------------------------------
    private String makeRaxLocCountyfipsFromIhfsRecord(LocationRecord ihfsRecord)
    {
        String raxCountyfips = null;
        
        // search through the countyfips Map to find a match for county|state
        // if a match is found then use its countynum
        // No match then set to default
        
        String countyfipsMapKey = createKeyForMap(ihfsRecord);
        
        raxCountyfips = (String) _ihfsCountyfipsMap.get(countyfipsMapKey);
        
        if (DbTable.isNull(raxCountyfips)) // not found in list
        {
            raxCountyfips = "XXX";
        }

        return raxCountyfips;
        
    }// end of makeRaxLocCountyfipsFromIhfsRecord method
    
    //----------------------------------------------------------------------------------------------
    private short setRaxLocPostFromIhfsRecord(LocationRecord ihfsRecord)
    {
        short raxLocationPost = 0;
        
        // set RAX location post field to 2 if IHFS Location post field is 1
        // else set it to ZERO
        if (ihfsRecord.getPost() == 1)
            raxLocationPost = 2;

        return raxLocationPost;
        
    }// end of setRaxLocPostFromIhfsRecord method

    //----------------------------------------------------------------------------------------------
    private String makeRaxLocRfcIdFromIhfsRecord(LocationRecord ihfsRecord)
    {
        String raxLocationRfcId = null;
        
        // to create a RAX location RFC id from an IHFS location record
        // just use the first 2 characters of the IHFS rfc field
        if (ihfsRecord.getRfc() != null)
        {
            if (ihfsRecord.getRfc().length() > 1)
            {
                raxLocationRfcId = ihfsRecord.getRfc().substring(0,2);
            }
        }

        return raxLocationRfcId;
        
    } // end of makeRaxLocRfcIdFromIhfsRecord method

    //----------------------------------------------------------------------------------------------
    private String makeRaxLocCountryIdFromIhfsRecord(LocationRecord ihfsRecord)
    {
        String raxLocationCountryId = null;
        
        // create a RAX Country ID from an IHFS location record
        // set Counrty ID to "US" and then see if it needs to be changed
        raxLocationCountryId = "US";
        
        if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("AB"))
            raxLocationCountryId = "CA";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("BC"))
            raxLocationCountryId = "CA";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("MB"))
            raxLocationCountryId = "CA";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("NB"))
            raxLocationCountryId = "CA";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("NF"))
            raxLocationCountryId = "CA";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("NT"))
            raxLocationCountryId = "CA";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("NS"))
            raxLocationCountryId = "CA";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("ON"))
            raxLocationCountryId = "CA";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("PE"))
            raxLocationCountryId = "CA";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("PQ"))
            raxLocationCountryId = "CA";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("SK"))
            raxLocationCountryId = "CA";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("YT"))
            raxLocationCountryId = "CA";

        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("AG"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("CM"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("CH"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("CI"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("CZ"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("CL"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("DF"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("DU"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("GU"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("GE"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("HD"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("JA"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("MX"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("MC"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("MR"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("NA"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("NL"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("OA"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("PU"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("QA"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("QR"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("SI"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("SO"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("TB"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("TM"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("TL"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("VL"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("VU"))
            raxLocationCountryId = "MX";
        else if (ihfsRecord.getState().substring(0,2).equalsIgnoreCase("ZA"))
            raxLocationCountryId = "MX";

        return raxLocationCountryId;
        
    }// end of makeRaxLocCountryIdFromIhfsRecord method
    
    //----------------------------------------------------------------------------------------------
    protected int insert(DbTable dbTable, DbRecord dbRecord) throws SQLException
    {
        RaxLocationTable table = (RaxLocationTable) dbTable;
        RaxLocationRecord record = (RaxLocationRecord) dbRecord;
        
        return table.insert(record);
    }

    //----------------------------------------------------------------------------------------------
    protected int update(DbTable dbTable, DbRecord dbRecordOld, DbRecord dbRecordNew) throws SQLException
    {
        RaxLocationTable table = (RaxLocationTable) dbTable;
        RaxLocationRecord recordOld = (RaxLocationRecord) dbRecordOld;
        RaxLocationRecord recordNew = (RaxLocationRecord) dbRecordNew;
        
        return table.update(recordOld, recordNew);
    }
   
} // end of LocationDifferenceMgr class
