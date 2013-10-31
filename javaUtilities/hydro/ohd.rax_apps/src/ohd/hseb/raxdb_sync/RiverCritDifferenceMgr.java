package ohd.hseb.raxdb_sync;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.db.DbRecord;
import ohd.hseb.db.DbTable;
import ohd.hseb.ihfsdb.generated.FloodcatRecord;
import ohd.hseb.ihfsdb.generated.RiverstatRecord;
import ohd.hseb.raxbase.model.RaxRiverCrit;
import ohd.hseb.raxdb.generated.RaxRiverCritRecord;
import ohd.hseb.raxdb.generated.RaxRiverCritTable;

public class RiverCritDifferenceMgr extends BaseDifferenceMgr implements IDifferenceMgr
{

    public RiverCritDifferenceMgr(RaxSyncDataMgr dm)
    {
        setDataMgr(dm);
        
        setupOutputFiles("RiverCrit");

    } // end of Constructor
    
    //----------------------------------------------------------------------------------------------
    public DiffSet findDifferences()
    {
        DiffSet tableDifferences = new DiffSet();

        List ihfsRecordList = null;
        List raxRecordList = null;
        List floodcatRecordList = null;
        
        RecordDifference oneDifference = null;

        logApplicationMessage("\n" + _dm.getDateTimeStamp() + " Begin Analyzing RiverCrit Table records...");
        logApplicationMessage("Apps_defaults Token adb_sync_rivercrit set to: " + _dm.getRiverCritPreference());
        
        ihfsRecordList = _dm.getIhfsRiverstatRecordList();
        _numberOfIhfsRecords = _numberOfIhfsRecords + ihfsRecordList.size();
        raxRecordList = _dm.getRaxRiverCritRecordList();
        floodcatRecordList = _dm.getIhfsFloodcatRecordList();

        //add the RAX records to a Map
        Map raxLidMap = new HashMap();
        for (int i=0; i < raxRecordList.size(); i++)
        {
            RaxRiverCritRecord record = (RaxRiverCritRecord) raxRecordList.get(i);
            raxLidMap.put(record.getLid(), record);
        } // end of raxRecordList for loop
        
        //add the IHFS records to a Map
        Map floodcatLidMap = new HashMap();
        for (int i=0; i < floodcatRecordList.size(); i++)
        {
            FloodcatRecord record = (FloodcatRecord) floodcatRecordList.get(i);
            floodcatLidMap.put(record.getLid(), record);
        } // end of floodcatRecordList for loop
        
        // loop through IHFS record list and check if same lid found in RAX
        // if not, this needs to be added as new to RAX
        // if same key found then check to see if they are "different"
        for (int i=0; i<ihfsRecordList.size(); i++)
        {
            List fieldDifferenceList = null;

            RiverstatRecord currentIhfsRecord = (RiverstatRecord) ihfsRecordList.get(i);
            String currentIhfsLid = currentIhfsRecord.getLid();
            RaxRiverCritRecord raxRecord = (RaxRiverCritRecord) raxLidMap.get(currentIhfsLid);
            FloodcatRecord floodcatRecord = (FloodcatRecord) floodcatLidMap.get(currentIhfsLid);

            if (raxRecord == null)  //not in the rax database
            {
                oneDifference = createDifferenceEntry(RecordDifferenceOriginType.NEW, currentIhfsRecord, null, null, floodcatRecord);
                tableDifferences.addDifference(oneDifference);
                _numberOfNewRecords++;
            }
            else //match found
            {
                fieldDifferenceList = getFieldDifferenceList(currentIhfsRecord, raxRecord, floodcatRecord);
                if (fieldDifferenceList.size() == 0) //same
                {
                    _numberOfSameRecords++;
                }
                else //different
                {
                    oneDifference = createDifferenceEntry(RecordDifferenceOriginType.MOD, currentIhfsRecord, raxRecord, fieldDifferenceList, floodcatRecord);
                    tableDifferences.addDifference(oneDifference);
                    _numberOfModRecords++;
                } 
            }

        } // end of ihfsRecordList for loop
                
        displayEndOfFindDifferencesMessage("RiverCrit");
        
        return tableDifferences;

    } // end of findDifferences method

    //----------------------------------------------------------------------------------------------
    private List getFieldDifferenceList(RiverstatRecord riverstatRecord, RaxRiverCritRecord raxRecord, FloodcatRecord floodcatRecord)
    {
        List fieldDifferenceList = new ArrayList();

        if (!DbTable.isNull(riverstatRecord.getPrimary_pe()))
        {
            checkAndAddToFieldDifferenceList("pe1", createRaxPeFromIhfsPrimaryPe(riverstatRecord.getPrimary_pe(), 1), raxRecord.getPe1(), fieldDifferenceList);
            checkAndAddToFieldDifferenceList("pe2", createRaxPeFromIhfsPrimaryPe(riverstatRecord.getPrimary_pe(), 2), raxRecord.getPe2(), fieldDifferenceList);
        }
        
        checkAndAddToFieldDifferenceList("bank", riverstatRecord.getBf(), raxRecord.getBank(), fieldDifferenceList);

        checkAndAddToFieldDifferenceList("flood", setFloodFromIhfsRecords(riverstatRecord, floodcatRecord), raxRecord.getFlood(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("floodf", setFloodfFromIhfsRecords(riverstatRecord, floodcatRecord), raxRecord.getFloodf(), fieldDifferenceList);

        checkAndAddToFieldDifferenceList("stream", riverstatRecord.getStream(), raxRecord.getStream(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("lat", riverstatRecord.getLat(), raxRecord.getLat(), 4, fieldDifferenceList);
        checkAndAddToFieldDifferenceList("lon", ConvertRaxLonFromIhfsLon(riverstatRecord.getLon()), raxRecord.getLon(), 4, fieldDifferenceList);
        checkAndAddToFieldDifferenceList("da", riverstatRecord.getDa(), raxRecord.getDa(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("mile", riverstatRecord.getMile(), raxRecord.getMile(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("zd", riverstatRecord.getZd(), raxRecord.getZd(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("vdatum", riverstatRecord.getVdatum(), raxRecord.getVdatum(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("cb", riverstatRecord.getCb(), raxRecord.getCb(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("level", riverstatRecord.getLevel(), raxRecord.getLevel(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("pool", riverstatRecord.getPool(), raxRecord.getPool(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("por", riverstatRecord.getPor(), raxRecord.getPor(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("tide", riverstatRecord.getTide(), raxRecord.getTide(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("backwater", riverstatRecord.getBackwater(), raxRecord.getBackwater(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("rrevise", displayDate(riverstatRecord.getRrevise()), displayDate(raxRecord.getRrevise()), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("rsource", riverstatRecord.getRsource(), raxRecord.getRsource(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("response_time", riverstatRecord.getResponse_time(), raxRecord.getResponse_time(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("threshold_runoff", riverstatRecord.getThreshold_runoff(), raxRecord.getThreshold_runoff(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("uhgdur", riverstatRecord.getUhgdur(), raxRecord.getUhgdur(), fieldDifferenceList);
        checkAndAddToFieldDifferenceList("remark", riverstatRecord.getRemark(), raxRecord.getRemark(), fieldDifferenceList);
        
        if(! _dm.getRiverCritPreference().equalsIgnoreCase("ACTION"))
        {
            checkAndAddToFieldDifferenceList("fis", riverstatRecord.getWstg(), raxRecord.getFis(), fieldDifferenceList);
            checkAndAddToFieldDifferenceList("fisf", riverstatRecord.getAction_flow(), raxRecord.getFisf(), fieldDifferenceList);
        }

        if(! _dm.getRiverCritPreference().equalsIgnoreCase("FIS"))
        {
            checkAndAddToFieldDifferenceList("action", riverstatRecord.getWstg(), raxRecord.getAction(), fieldDifferenceList);
            checkAndAddToFieldDifferenceList("actionf", riverstatRecord.getAction_flow(), raxRecord.getActionf(), fieldDifferenceList);
        }

        // if there is not a corressponding floodcat record for this riverstat record then don't compare these fields
        if (floodcatRecord != null)
        {
            checkAndAddToFieldDifferenceList("modflood", floodcatRecord.getModerate_stage(), raxRecord.getModflood(), fieldDifferenceList);
            checkAndAddToFieldDifferenceList("majflood", floodcatRecord.getMajor_stage(), raxRecord.getMajflood(), fieldDifferenceList);
            checkAndAddToFieldDifferenceList("modfloodf", floodcatRecord.getModerate_flow(), raxRecord.getModfloodf(), fieldDifferenceList);
            checkAndAddToFieldDifferenceList("majfloodf", floodcatRecord.getMajor_flow(), raxRecord.getMajfloodf(), fieldDifferenceList);          
        }

        return fieldDifferenceList;

    } // end of getFieldDifferenceList method
    
    //----------------------------------------------------------------------------------------------
    private RecordDifference createDifferenceEntry(RecordDifferenceOriginType type, RiverstatRecord ihfsRecord,
            final RaxRiverCritRecord raxRecord, List fieldDiffList, FloodcatRecord floodcatRecord)
    {
        RaxRiverCritRecord derivedRaxRecord = null;
        RaxRiverCrit ihfsRaxRiverCrit = null;

        derivedRaxRecord = new RaxRiverCritRecord();

        derivedRaxRecord.setLid(ihfsRecord.getLid());

        derivedRaxRecord.setPe1(createRaxPeFromIhfsPrimaryPe(ihfsRecord.getPrimary_pe(), 1));
        derivedRaxRecord.setPe2(createRaxPeFromIhfsPrimaryPe(ihfsRecord.getPrimary_pe(), 2));

        derivedRaxRecord.setVdtime(System.currentTimeMillis());    // set to Today's date
        derivedRaxRecord.setLowscreen(DbTable.getNullDouble());    // set to null
        derivedRaxRecord.setSigrate(DbTable.getNullDouble());    // set to null
        derivedRaxRecord.setScreenrate(DbTable.getNullDouble());    // set to null
        derivedRaxRecord.setAlert(DbTable.getNullDouble());    // set to null
        derivedRaxRecord.setBank(ihfsRecord.getBf());
        derivedRaxRecord.setFlood(setFloodFromIhfsRecords(ihfsRecord, floodcatRecord));
        derivedRaxRecord.setRecord(DbTable.getNullDouble());    // set to null
        derivedRaxRecord.setHighscreen(DbTable.getNullDouble());    // set to null
        derivedRaxRecord.setDamscreen(DbTable.getNullDouble());    // set to null
        derivedRaxRecord.setLowscreenf(DbTable.getNullDouble());    // set to null
        derivedRaxRecord.setSigratef(DbTable.getNullDouble());    // set to null
        derivedRaxRecord.setScreenratef(DbTable.getNullDouble());    // set to null
        derivedRaxRecord.setAlertf(DbTable.getNullDouble());    // set to null
        derivedRaxRecord.setBankf(DbTable.getNullDouble());    // set to null
        derivedRaxRecord.setFloodf(setFloodfFromIhfsRecords(ihfsRecord, floodcatRecord));
        derivedRaxRecord.setRecordf(DbTable.getNullDouble());    // set to null
        derivedRaxRecord.setHighscreenf(DbTable.getNullDouble());    // set to null
        derivedRaxRecord.setDamscreenf(DbTable.getNullDouble());    // set to null
        derivedRaxRecord.setSigratet(DbTable.getNullDouble());    // set to null
        derivedRaxRecord.setScreenratet(DbTable.getNullDouble());    // set to null
        derivedRaxRecord.setLowscreenq(DbTable.getNullString());    // set to null
        derivedRaxRecord.setSigrateq(DbTable.getNullString());    // set to null
        derivedRaxRecord.setScreenrateq(DbTable.getNullString());    // set to null
        derivedRaxRecord.setFisq(DbTable.getNullString());    // set to null
        derivedRaxRecord.setActionq(DbTable.getNullString());    // set to null
        derivedRaxRecord.setAlertq(DbTable.getNullString());    // set to null
        derivedRaxRecord.setBankq(DbTable.getNullString());    // set to null
        derivedRaxRecord.setFloodq(DbTable.getNullString());    // set to null
        derivedRaxRecord.setModfloodq(DbTable.getNullString());    // set to null
        derivedRaxRecord.setMajfloodq(DbTable.getNullString());    // set to null
        derivedRaxRecord.setRecordq(DbTable.getNullString());    // set to null
        derivedRaxRecord.setHighscreenq(DbTable.getNullString());    // set to null
        derivedRaxRecord.setDamscreenq(DbTable.getNullString());    // set to null
        derivedRaxRecord.setStream(makeMixedCase(ihfsRecord.getStream()));
        derivedRaxRecord.setLat(ihfsRecord.getLat());
        derivedRaxRecord.setLon(ConvertRaxLonFromIhfsLon(ihfsRecord.getLon()));
        derivedRaxRecord.setDa(ihfsRecord.getDa());
        derivedRaxRecord.setMile(ihfsRecord.getMile());
        derivedRaxRecord.setZd(ihfsRecord.getZd());
        derivedRaxRecord.setVdatum(ihfsRecord.getVdatum());
        derivedRaxRecord.setCb(ihfsRecord.getCb());
        derivedRaxRecord.setLevel(ihfsRecord.getLevel());
        derivedRaxRecord.setPool(ihfsRecord.getPool());
        derivedRaxRecord.setPor(ihfsRecord.getPor());
        derivedRaxRecord.setTide(ihfsRecord.getTide());
        derivedRaxRecord.setBackwater(ihfsRecord.getBackwater());
        derivedRaxRecord.setRrevise(ihfsRecord.getRrevise());
        derivedRaxRecord.setRsource(ihfsRecord.getRsource());
        derivedRaxRecord.setResponse_time(ihfsRecord.getResponse_time());
        derivedRaxRecord.setThreshold_runoff(ihfsRecord.getThreshold_runoff());
        derivedRaxRecord.setUhgdur(ihfsRecord.getUhgdur());
        derivedRaxRecord.setRemark(ihfsRecord.getRemark());

        // set the RAX fis, fisf, action and actionf columns to the IHFS values
        derivedRaxRecord.setFis(ihfsRecord.getWstg());
        derivedRaxRecord.setAction(ihfsRecord.getWstg());
        derivedRaxRecord.setFisf(ihfsRecord.getAction_flow());
        derivedRaxRecord.setActionf(ihfsRecord.getAction_flow());
        // check the token settings (ACTION, FIS or BOTH) to determine if any get set to NULL
        if (_dm.getRiverCritPreference().equalsIgnoreCase("ACTION"))
        {
            derivedRaxRecord.setFis(DbTable.getNullDouble());
            derivedRaxRecord.setFisf(DbTable.getNullDouble());
        }
        else if (_dm.getRiverCritPreference().equalsIgnoreCase("FIS"))
        {
            derivedRaxRecord.setAction(DbTable.getNullDouble());
            derivedRaxRecord.setActionf(DbTable.getNullDouble());
        }

        // if there is not a coressponding floodcat record for this riverstat record then set the values to null
        if (floodcatRecord != null)
        {
            derivedRaxRecord.setModflood(floodcatRecord.getModerate_stage());
            derivedRaxRecord.setMajflood(floodcatRecord.getMajor_stage());
            derivedRaxRecord.setModfloodf(floodcatRecord.getModerate_flow());
            derivedRaxRecord.setMajfloodf(floodcatRecord.getMajor_flow());
        }
        else
        {
            derivedRaxRecord.setModflood(DbTable.getNullDouble());    // set to null
            derivedRaxRecord.setMajflood(DbTable.getNullDouble());    // set to null
            derivedRaxRecord.setModfloodf(DbTable.getNullDouble());    // set to null
            derivedRaxRecord.setMajfloodf(DbTable.getNullDouble());    // set to null
        }

        // create a RaxRiverCrit object from a RaxRiverCritRecord which was 
        // derived from an IHFS Riverstat and FloodCat Record 
        ihfsRaxRiverCrit = new RaxRiverCrit(derivedRaxRecord);

        if (type == RecordDifferenceOriginType.MOD)
        {
            derivedRaxRecord = new RaxRiverCritRecord(raxRecord);            
        }

        // Place the RaxRiverCrit and Riverstat objects into RiverCritHolder object which extends the dbRecord class
        // in order to pass them into the RecordDifference constructor
        RiverCritHolder ihfsRiverCritHolder = new RiverCritHolder(ihfsRaxRiverCrit, ihfsRecord);      

        RecordDifference oneDifferenceRecord = new RecordDifference(type, this, ihfsRiverCritHolder, derivedRaxRecord, fieldDiffList);
        return oneDifferenceRecord;

    } // end of createDifferenceEntry method

    //----------------------------------------------------------------------------------------------
    public void reportDifferences(RecordDifference oneRecordDifference)
    {
        FieldDifference oneFieldDifference = new FieldDifference();

        RiverCritHolder riverCritHolder = (RiverCritHolder) oneRecordDifference.getIhfsRecord();
        RiverstatRecord ihfsRecord = (RiverstatRecord) riverCritHolder.getRiverstatRecord();
        
        RaxRiverCritRecord raxRecord = (RaxRiverCritRecord) oneRecordDifference.getRaxRecord();

        if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
        {
            logRecordDifferenceMessage("\nDifferences found in Table = RiverCrit" + " for Primary key = |" + raxRecord.getLid()
                                            + "|" + raxRecord.getPe1() + raxRecord.getPe2() + "|" + displayDate(raxRecord.getVdtime()) + "|");
            for (int j=0; j<oneRecordDifference.getFieldDifferenceList().size(); j++)
            {
                oneFieldDifference = (FieldDifference) oneRecordDifference.getFieldDifferenceList().get(j); 
                logRecordDifferenceMessage("Column = " + oneFieldDifference.getName() + "  IHFS_value = "
                        + oneFieldDifference.getIhfsValue() + "  Rax_value = " + oneFieldDifference.getRaxValue());
            }
        }
        else
        {

            logNewRecordMessage("\nA new RiverCrit record will need to be created in RAX db for Primary key = |" + raxRecord.getLid() +
                                "|" + raxRecord.getPe1() + raxRecord.getPe2() + "|" + displayDate(raxRecord.getVdtime()) + "|" +
                                "\nlid = " + raxRecord.getLid() +
                                "\npe1 = " + raxRecord.getPe1() + " pe2 = " + raxRecord.getPe2() +
                                "\nvdtime = " + displayDate(raxRecord.getVdtime()) +
                                "\nlowscreen = " + displayDouble(raxRecord.getLowscreen()) +
                                "\nsigrate = " + displayDouble(raxRecord.getSigrate()) +
                                "\nscreenrate = " + displayDouble(raxRecord.getScreenrate()) +
                                "\nfis = " + displayDouble(raxRecord.getFis()) +
                                "\naction = " + displayDouble(raxRecord.getAction()) +
                                "\nalert = " + displayDouble(raxRecord.getAlert()) +
                                "\nbank = " + displayDouble(raxRecord.getBank()) +
                                "\nflood = " + displayDouble(raxRecord.getFlood()) +
                                "\nmodflood = " + displayDouble(raxRecord.getModflood()) +
                                "\nmajflood = " + displayDouble(raxRecord.getMajflood()) +
                                "\nrecord = " + displayDouble(raxRecord.getRecord()) +
                                "\nhighscreen = " + displayDouble(raxRecord.getHighscreen()) +
                                "\ndamscreen = " + displayDouble(raxRecord.getDamscreen()) +
                                "\nlowscreenf = " + displayDouble(raxRecord.getLowscreenf()) +
                                "\nsigratef = " + displayDouble(raxRecord.getSigratef()) +
                                "\nscreenratef = " + displayDouble(raxRecord.getScreenratef()) +
                                "\nfisf = " + displayDouble(raxRecord.getFisf()) +
                                "\nactionf = " + displayDouble(raxRecord.getActionf()) +
                                "\nalertf = " + displayDouble(raxRecord.getAlertf()) +
                                "\nbankf = " + displayDouble(raxRecord.getBankf()) +
                                "\nfloodf = " + displayDouble(raxRecord.getFloodf()) +
                                "\nmodfloodf = " + displayDouble(raxRecord.getModfloodf()) +
                                "\nmajfloodf = " + displayDouble(raxRecord.getMajfloodf()) +
                                "\nrecordf = " + displayDouble(raxRecord.getRecordf()) +
                                "\nhighscreenf = " + displayDouble(raxRecord.getHighscreenf()) +
                                "\ndamscreenf = " + displayDouble(raxRecord.getDamscreenf()) +
                                "\nsigratet = " + displayDouble(raxRecord.getSigratet()) +
                                "\nscreenratet = " + displayDouble(raxRecord.getScreenratet()) +
                                "\nlowscreenq = " + raxRecord.getLowscreenq() +
                                "\nsigrateq = " + raxRecord.getSigrateq() +
                                "\nscreenrateq = " + raxRecord.getScreenrateq() +
                                "\nfisq = " + raxRecord.getFisq() +
                                "\nactionq = " + raxRecord.getActionq() +
                                "\nalertq = " + raxRecord.getAlertq() +
                                "\nbankq = " + raxRecord.getBankq() +
                                "\nfloodq = " + raxRecord.getFloodq() +
                                "\nmodfloodq = " + raxRecord.getModfloodq() +
                                "\nmajfloodq = " + raxRecord.getMajfloodq() +
                                "\nrecordq = " + raxRecord.getRecordq() +
                                "\nhighscreenq = " + raxRecord.getHighscreenq() +
                                "\ndamscreenq = " + raxRecord.getDamscreenq() +
                                "\nstream = " + raxRecord.getStream() +
                                "\nlat = " + displayLatLon(raxRecord.getLat()) +
                                "\nlon = " + displayLatLon(raxRecord.getLon()) +
                                "\nda = " + displayDouble(raxRecord.getDa()) +
                                "\nmile = " + displayDouble(raxRecord.getMile()) +
                                "\nzd = " + displayDouble(raxRecord.getZd()) +
                                "\nvdatum = " + raxRecord.getVdatum() +
                                "\ncb = " + displayDouble(raxRecord.getCb()) +
                                "\nlevel = " + raxRecord.getLevel() +
                                "\npool = " + displayDouble(raxRecord.getPool()) +
                                "\npor = " + raxRecord.getPor() +
                                "\ntide = " + raxRecord.getTide() +
                                "\nbackwater = " + raxRecord.getBackwater() +
                                "\nrrevise = " + displayDate(raxRecord.getRrevise()) +
                                "\nrsource = " + raxRecord.getRsource() +
                                "\nresponse_time = " + displayDouble(raxRecord.getResponse_time()) +
                                "\nthreshold_runnoff = " + displayDouble(raxRecord.getThreshold_runoff()) +
                                "\nuhgdur = " + displayInt(raxRecord.getUhgdur()) +
                                "\nremark = " + raxRecord.getRemark());
            
            logDebugMessage("\nA new RiverCrit record will need to be created in RAX db for Primary key = |" + raxRecord.getLid() +
                            "|" + raxRecord.getPe1() + raxRecord.getPe2() + "|" + displayDate(raxRecord.getVdtime()) + "|" +
                            "\nusing the following information from the IHFS db:" +
                            "\nlid = " + raxRecord.getLid() + "   IHFS_lid = " + ihfsRecord.getLid() +
                            "\npe1 = " + raxRecord.getPe1() + " pe2 = " + raxRecord.getPe2() + "   IHFS_primary_pe = " + ihfsRecord.getPrimary_pe() +
                            "\nvdtime = " + displayDate(raxRecord.getVdtime()) +
                            "\nlowscreen = " + displayDouble(raxRecord.getLowscreen()) +
                            "\nsigrate = " + displayDouble(raxRecord.getSigrate()) +
                            "\nscreenrate = " + displayDouble(raxRecord.getScreenrate()) +
                            "\nfis = " + displayDouble(raxRecord.getFis()) + "   IHFS_wstg = " + displayDouble(ihfsRecord.getWstg()) +
                            "\naction = " + displayDouble(raxRecord.getAction()) + "   IHFS_wstg = " + displayDouble(ihfsRecord.getWstg()) +
                            "\nalert = " + displayDouble(raxRecord.getAlert()) +
                            "\nbank = " + displayDouble(raxRecord.getBank()) + "   IHFS_bf = " + displayDouble(ihfsRecord.getBf()) +
                            "\nflood = " + displayDouble(raxRecord.getFlood()) + "   IHFS_fs = " + displayDouble(ihfsRecord.getFs()) +
                            "\nmodflood = " + displayDouble(raxRecord.getModflood()) +
                            "\nmajflood = " + displayDouble(raxRecord.getMajflood()) +
                            "\nrecord = " + displayDouble(raxRecord.getRecord()) +
                            "\nhighscreen = " + displayDouble(raxRecord.getHighscreen()) +
                            "\ndamscreen = " + displayDouble(raxRecord.getDamscreen()) +
                            "\nlowscreenf = " + displayDouble(raxRecord.getLowscreenf()) +
                            "\nsigratef = " + displayDouble(raxRecord.getSigratef()) +
                            "\nscreenratef = " + displayDouble(raxRecord.getScreenratef()) +
                            "\nfisf = " + displayDouble(raxRecord.getFisf()) + "   IHFS_action_flow = " + displayDouble(ihfsRecord.getAction_flow()) +
                            "\nactionf = " + displayDouble(raxRecord.getActionf()) + "   IHFS_action_flow = " + displayDouble(ihfsRecord.getAction_flow()) +
                            "\nalertf = " + displayDouble(raxRecord.getAlertf()) +
                            "\nbankf = " + displayDouble(raxRecord.getBankf()) +
                            "\nfloodf = " + displayDouble(raxRecord.getFloodf()) + "   IHFS_fq = " + displayDouble(ihfsRecord.getFq()) +
                            "\nmodfloodf = " + displayDouble(raxRecord.getModfloodf()) +
                            "\nmajfloodf = " + displayDouble(raxRecord.getMajfloodf()) +
                            "\nrecordf = " + displayDouble(raxRecord.getRecordf()) +
                            "\nhighscreenf = " + displayDouble(raxRecord.getHighscreenf()) +
                            "\ndamscreenf = " + displayDouble(raxRecord.getDamscreenf()) +
                            "\nsigratet = " + displayDouble(raxRecord.getSigratet()) +
                            "\nscreenratet = " + displayDouble(raxRecord.getScreenratet()) +
                            "\nlowscreenq = " + raxRecord.getLowscreenq() +
                            "\nsigrateq = " + raxRecord.getSigrateq() +
                            "\nscreenrateq = " + raxRecord.getScreenrateq() +
                            "\nfisq = " + raxRecord.getFisq() +
                            "\nactionq = " + raxRecord.getActionq() +
                            "\nalertq = " + raxRecord.getAlertq() +
                            "\nbankq = " + raxRecord.getBankq() +
                            "\nfloodq = " + raxRecord.getFloodq() +
                            "\nmodfloodq = " + raxRecord.getModfloodq() +
                            "\nmajfloodq = " + raxRecord.getMajfloodq() +
                            "\nrecordq = " + raxRecord.getRecordq() +
                            "\nhighscreenq = " + raxRecord.getHighscreenq() +
                            "\ndamscreenq = " + raxRecord.getDamscreenq() +
                            "\nstream = " + raxRecord.getStream() + "   IHFS_stream = " + ihfsRecord.getStream() +
                            "\nlat = " + displayLatLon(raxRecord.getLat()) + "   IHFS_lat = " + displayLatLon(ihfsRecord.getLat()) +
                            "\nlon = " + displayLatLon(raxRecord.getLon()) + "   IHFS_lon = " + displayLatLon(ihfsRecord.getLon()) +
                            "\nda = " + displayDouble(raxRecord.getDa()) + "   IHFS_da = " + displayDouble(ihfsRecord.getDa()) +
                            "\nmile = " + displayDouble(raxRecord.getMile()) + "   IHFS_mile = " + displayDouble(ihfsRecord.getMile()) +
                            "\nzd = " + displayDouble(raxRecord.getZd()) + "   IHFS_zd = " + displayDouble(ihfsRecord.getZd()) +
                            "\nvdatum = " + raxRecord.getVdatum() + "   IHFS_vdatum = " + ihfsRecord.getVdatum() +
                            "\ncb = " + displayDouble(raxRecord.getCb()) + "   IHFS_cb = " + displayDouble(ihfsRecord.getCb()) +
                            "\nlevel = " + raxRecord.getLevel() + "   IHFS_level = " + ihfsRecord.getLevel() +
                            "\npool = " + displayDouble(raxRecord.getPool()) + "   IHFS_pool = " + displayDouble(ihfsRecord.getPool()) +
                            "\npor = " + raxRecord.getPor() + "   IHFS_por = " + ihfsRecord.getPor() +
                            "\ntide = " + raxRecord.getTide() + "   IHFS_tide = " + ihfsRecord.getTide() +
                            "\nbackwater = " + raxRecord.getBackwater() + "   IHFS_backwater = " + ihfsRecord.getBackwater() +
                            "\nrrevise = " + displayDate(raxRecord.getRrevise()) + "   IHFS_rrevise = " + displayDate(ihfsRecord.getRrevise()) +
                            "\nrsource = " + raxRecord.getRsource() + "   IHFS_rsource = " + ihfsRecord.getRsource() +
                            "\nresponse_time = " + displayDouble(raxRecord.getResponse_time()) + "   IHFS_response_time = " + displayDouble(ihfsRecord.getResponse_time()) +
                            "\nthreshold_runnoff = " + displayDouble(raxRecord.getThreshold_runoff()) + "   IHFS_threshold_runoff = " + displayDouble(ihfsRecord.getThreshold_runoff()) +
                            "\nuhgdur = " + displayInt(raxRecord.getUhgdur()) + "   IHFS_uhgdur = " + displayInt(ihfsRecord.getUhgdur()) +
                            "\nremark = " + raxRecord.getRemark() + "   IHFS_remark = " + ihfsRecord.getRemark());
        }

    } // end of reportDifferences method

    //----------------------------------------------------------------------------------------------
    public int processDifferences(RecordDifference oneRecordDifference)
    {
        int numberOfFailedDbUpdates = 0;
        RaxRiverCritTable raxTable = null;
        RaxRiverCritRecord newRaxRecord = null;
        
        RiverCritHolder riverCritHolder = (RiverCritHolder) oneRecordDifference.getIhfsRecord();
        RiverstatRecord ihfsRecord = (RiverstatRecord) riverCritHolder.getRiverstatRecord();

        RaxRiverCritRecord raxRecord = (RaxRiverCritRecord) oneRecordDifference.getRaxRecord();

        raxTable = _dm.getRaxRiverCritTable();

        if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
        {
            newRaxRecord =  new RaxRiverCritRecord(raxRecord);

            if (!DbTable.isNull(ihfsRecord.getPrimary_pe()))
            {
                newRaxRecord.setPe1(createRaxPeFromIhfsPrimaryPe(ihfsRecord.getPrimary_pe(), 1));
                newRaxRecord.setPe2(createRaxPeFromIhfsPrimaryPe(ihfsRecord.getPrimary_pe(), 2));
            }

            newRaxRecord.setVdtime(System.currentTimeMillis());    // set to Today's date
            newRaxRecord.setBank(ihfsRecord.getBf());
            newRaxRecord.setStream(makeMixedCase(ihfsRecord.getStream()));
            newRaxRecord.setLat(ihfsRecord.getLat());
            newRaxRecord.setLon(ConvertRaxLonFromIhfsLon(ihfsRecord.getLon()));
            newRaxRecord.setDa(ihfsRecord.getDa());
            newRaxRecord.setMile(ihfsRecord.getMile());
            newRaxRecord.setZd(ihfsRecord.getZd());
            newRaxRecord.setVdatum(ihfsRecord.getVdatum());
            newRaxRecord.setCb(ihfsRecord.getCb());
            newRaxRecord.setLevel(ihfsRecord.getLevel());
            newRaxRecord.setPool(ihfsRecord.getPool());
            newRaxRecord.setPor(ihfsRecord.getPor());
            newRaxRecord.setTide(ihfsRecord.getTide());
            newRaxRecord.setBackwater(ihfsRecord.getBackwater());
            newRaxRecord.setRrevise(ihfsRecord.getRrevise());
            newRaxRecord.setRsource(ihfsRecord.getRsource());
            newRaxRecord.setResponse_time(ihfsRecord.getResponse_time());
            newRaxRecord.setThreshold_runoff(ihfsRecord.getThreshold_runoff());
            newRaxRecord.setUhgdur(ihfsRecord.getUhgdur());
            newRaxRecord.setRemark(ihfsRecord.getRemark());
            
            // since we only story one IHFS record type (in this case RiverStat) in a RecordDifference object
            // we need to extract the IHFS value (stored as a Srting) to use for fields that come from FloodCat
            // and convert them to the proper data type (double) to overwrite the RiverCrit value
            FieldDifference oneFieldDifference = new FieldDifference();
            for (int j=0; j<oneRecordDifference.getFieldDifferenceList().size(); j++)
            {
                oneFieldDifference = (FieldDifference) oneRecordDifference.getFieldDifferenceList().get(j);

                if (oneFieldDifference.getName().equalsIgnoreCase("flood"))
                {
                    if (oneFieldDifference.getIhfsValue().equalsIgnoreCase("null"))
                        newRaxRecord.setFlood(DbTable.getNullDouble());    // set to null
                    else
                        newRaxRecord.setFlood(new Double (oneFieldDifference.getIhfsValue()).doubleValue());
                }
                else if (oneFieldDifference.getName().equalsIgnoreCase("floodf"))
                {
                    if (oneFieldDifference.getIhfsValue().equalsIgnoreCase("null"))
                        newRaxRecord.setFloodf(DbTable.getNullDouble());    // set to null
                    else
                        newRaxRecord.setFloodf(new Double (oneFieldDifference.getIhfsValue()).doubleValue());
                }
                else if (oneFieldDifference.getName().equalsIgnoreCase("modflood"))
                {
                    if (oneFieldDifference.getIhfsValue().equalsIgnoreCase("null"))
                        newRaxRecord.setModflood(DbTable.getNullDouble());    // set to null
                    else
                        newRaxRecord.setModflood(new Double (oneFieldDifference.getIhfsValue()).doubleValue());
                }
                else if (oneFieldDifference.getName().equalsIgnoreCase("modfloodf"))
                {
                    if (oneFieldDifference.getIhfsValue().equalsIgnoreCase("null"))
                        newRaxRecord.setModfloodf(DbTable.getNullDouble());    // set to null
                    else
                        newRaxRecord.setModfloodf(new Double (oneFieldDifference.getIhfsValue()).doubleValue());
                }
                else if (oneFieldDifference.getName().equalsIgnoreCase("majflood"))
                {
                    if (oneFieldDifference.getIhfsValue().equalsIgnoreCase("null"))
                        newRaxRecord.setMajflood(DbTable.getNullDouble());    // set to null
                    else
                        newRaxRecord.setMajflood(new Double (oneFieldDifference.getIhfsValue()).doubleValue());
                }
                else if (oneFieldDifference.getName().equalsIgnoreCase("majfloodf"))
                {
                    if (oneFieldDifference.getIhfsValue().equalsIgnoreCase("null"))
                        newRaxRecord.setMajfloodf(DbTable.getNullDouble());    // set to null
                    else
                        newRaxRecord.setMajfloodf(new Double (oneFieldDifference.getIhfsValue()).doubleValue());
                }
                
            }  // for loop
            
            // check the app-defaults token value to determine if these values are overwritten
            if(! _dm.getRiverCritPreference().equalsIgnoreCase("ACTION"))
            {
                newRaxRecord.setFis(ihfsRecord.getWstg());
                newRaxRecord.setFisf(ihfsRecord.getAction_flow());                
            }

            if(! _dm.getRiverCritPreference().equalsIgnoreCase("FIS"))
            {
                newRaxRecord.setAction(ihfsRecord.getWstg());
                newRaxRecord.setActionf(ihfsRecord.getAction_flow());                
            }
            
            numberOfFailedDbUpdates = processRaxInsertUpdate(INSERT, "RiverCrit", raxTable, newRaxRecord, null);
        }
        else
        {
            numberOfFailedDbUpdates = processRaxInsertUpdate(INSERT, "RiverCrit", raxTable, raxRecord, null);
        }
        
        return numberOfFailedDbUpdates;

    } // end of processDifferences method

    //----------------------------------------------------------------------------------------------
    protected String formatColumnsForInsertUpdateDisplay(DbRecord dbRecord)
    {
        RaxRiverCritRecord raxRecord = (RaxRiverCritRecord) dbRecord;
        
        String formatedString = new String("|" + raxRecord.getLid() + "|"
                                               + raxRecord.getPe1() + "|"
                                               + raxRecord.getPe2() + "|"
                                               + displayDate(raxRecord.getVdtime()) + "|"
                                               + displayDouble(raxRecord.getLowscreen()) + "|"
                                               + displayDouble(raxRecord.getSigrate()) + "|"
                                               + displayDouble(raxRecord.getScreenrate()) + "|"
                                               + displayDouble(raxRecord.getFis()) + "|"
                                               + displayDouble(raxRecord.getAction()) + "|"
                                               + displayDouble(raxRecord.getAlert()) + "|"
                                               + displayDouble(raxRecord.getBank()) + "|"
                                               + displayDouble(raxRecord.getFlood()) + "|"
                                               + displayDouble(raxRecord.getModflood()) + "|"
                                               + displayDouble(raxRecord.getMajflood()) + "|"
                                               + displayDouble(raxRecord.getRecord()) + "|"
                                               + displayDouble(raxRecord.getHighscreen()) + "|"
                                               + displayDouble(raxRecord.getDamscreen()) + "|"
                                               + displayDouble(raxRecord.getLowscreenf()) + "|"
                                               + displayDouble(raxRecord.getSigratef()) + "|"
                                               + displayDouble(raxRecord.getScreenratef()) + "|"
                                               + displayDouble(raxRecord.getFisf()) + "|"
                                               + displayDouble(raxRecord.getActionf()) + "|"
                                               + displayDouble(raxRecord.getAlertf()) + "|"
                                               + displayDouble(raxRecord.getBankf()) + "|"
                                               + displayDouble(raxRecord.getFloodf()) + "|"
                                               + displayDouble(raxRecord.getModfloodf()) + "|"
                                               + displayDouble(raxRecord.getMajfloodf()) + "|"
                                               + displayDouble(raxRecord.getRecordf()) + "|"
                                               + displayDouble(raxRecord.getHighscreenf()) + "|"
                                               + displayDouble(raxRecord.getDamscreenf()) + "|"
                                               + displayDouble(raxRecord.getSigratet()) + "|"
                                               + displayDouble(raxRecord.getScreenratet()) + "|"
                                               + raxRecord.getLowscreenq() + "|"
                                               + raxRecord.getSigrateq() + "|"
                                               + raxRecord.getScreenrateq() + "|"
                                               + raxRecord.getFisq() + "|"
                                               + raxRecord.getActionq() + "|"
                                               + raxRecord.getAlertq() + "|"
                                               + raxRecord.getBankq() + "|"
                                               + raxRecord.getFloodq() + "|"
                                               + raxRecord.getModfloodq() + "|"
                                               + raxRecord.getMajfloodq() + "|"
                                               + raxRecord.getRecordq() + "|"
                                               + raxRecord.getHighscreenq() + "|"
                                               + raxRecord.getDamscreenq() + "|"
                                               + raxRecord.getStream() + "|"
                                               + displayLatLon(raxRecord.getLat()) + "|"
                                               + displayLatLon(raxRecord.getLon()) + "|"
                                               + displayDouble(raxRecord.getDa()) + "|"
                                               + displayDouble(raxRecord.getMile()) + "|"
                                               + displayDouble(raxRecord.getZd()) + "|"
                                               + raxRecord.getVdatum() + "|"
                                               + displayDouble(raxRecord.getCb()) + "|"
                                               + raxRecord.getLevel() + "|"
                                               + displayDouble(raxRecord.getPool()) + "|"
                                               + raxRecord.getPor() + "|"
                                               + raxRecord.getTide() + "|"
                                               + raxRecord.getBackwater() + "|"
                                               + displayDate(raxRecord.getRrevise()) + "|"
                                               + raxRecord.getRsource() + "|"
                                               + displayDouble(raxRecord.getResponse_time()) + "|"
                                               + displayDouble(raxRecord.getThreshold_runoff()) + "|"
                                               + displayInt(raxRecord.getUhgdur()) + "|"
                                               + raxRecord.getRemark() + "|");
        
        return formatedString;

    } // end formatColumnsForInsertUpdateDisplay method
    


    //----------------------------------------------------------------------------------------------
    private double setFloodFromIhfsRecords(RiverstatRecord riverstatRecord, FloodcatRecord floodcatRecord)
    {
        double riverCritFlood = DbTable.getNullDouble();    // set to null;
        
        // set RAX rivercrit flood field to 
        // 1) fs from IHFS RiverStat if not NULL
        // 2) minor_stage from FloodCat
        
        if (!DbTable.isNull(riverstatRecord.getFs()))
        {
            riverCritFlood = riverstatRecord.getFs();
        }
        else
        {
            if (floodcatRecord != null)
                riverCritFlood = floodcatRecord.getMinor_stage();
        }

        return riverCritFlood;
        
    }// end of setFloodFromIhfsRecords method

    //----------------------------------------------------------------------------------------------
    private double setFloodfFromIhfsRecords(RiverstatRecord riverstatRecord, FloodcatRecord floodcatRecord)
    {
        double riverCritFloodf = DbTable.getNullDouble();    // set to null;
        
        // set RAX rivercrit flood field to 
        // 1) fq from IHFS RiverStat if not NULL
        // 2) minor_flow from FloodCat
        
        if (!DbTable.isNull(riverstatRecord.getFq()))
        {
            riverCritFloodf = riverstatRecord.getFq();
        }
        else
        {
            if (floodcatRecord != null)
                riverCritFloodf = floodcatRecord.getMinor_flow();
        }

        return riverCritFloodf;
        
    }// end of setFloodfFromIhfsRecords method

    //----------------------------------------------------------------------------------------------
    private String createRaxPeFromIhfsPrimaryPe(String primaryPe, int subColumn)
    {
        String raxColumnValue = null;
        
        // by default set RAX pe1 to "H" and pe2 to "G"
        // if IHFS primary_pe is not null then set
        // RAX pe1 to first character of the primary_pe or pe2 to second character of the primary_pe
        if (subColumn == 1)
            raxColumnValue = "H";
        else
            raxColumnValue = "G";                                    

        if (!DbTable.isNull(primaryPe))
        {
            if (primaryPe.length() > 1)
            {
                if (subColumn == 1)
                    raxColumnValue = primaryPe.substring(0,1);
                else
                    raxColumnValue = primaryPe.substring(1,2);
            }
        }

        return raxColumnValue;

    }// end of createRaxPeFromIhfsPrimaryPe method

    //----------------------------------------------------------------------------------------------
    protected int insert(DbTable dbTable, DbRecord dbRecord) throws SQLException
    {
        RaxRiverCritTable table = (RaxRiverCritTable) dbTable;
        RaxRiverCritRecord record = (RaxRiverCritRecord) dbRecord;
        
        return table.insert(record);
    }

    //----------------------------------------------------------------------------------------------
    protected int update(DbTable dbTable, DbRecord dbRecordOld, DbRecord dbRecordNew) throws SQLException
    {
        RaxRiverCritTable table = (RaxRiverCritTable) dbTable;
        RaxRiverCritRecord recordOld = (RaxRiverCritRecord) dbRecordOld;
        RaxRiverCritRecord recordNew = (RaxRiverCritRecord) dbRecordNew;
        
        return table.update(recordOld, recordNew);
    }

} // end of RiverCritDifferenceMgr class
