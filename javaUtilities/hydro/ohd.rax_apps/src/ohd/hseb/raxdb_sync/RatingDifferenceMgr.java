package ohd.hseb.raxdb_sync;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.db.DbRecord;
import ohd.hseb.db.DbTable;

import ohd.hseb.ihfsdb.generated.RatingRecord;
import ohd.hseb.ihfsdb.generated.RatingShiftRecord;
import ohd.hseb.ihfsdb.generated.RiverstatRecord;
import ohd.hseb.raxbase.db.CustomRaxRatingTable;
import ohd.hseb.raxbase.model.RaxRating;
import ohd.hseb.raxbase.model.RaxRatingPoint;
import ohd.hseb.raxbase.model.RaxRatingShift;
import ohd.hseb.raxdb.generated.RaxRatingRecord;
import ohd.hseb.raxdb.generated.RaxRatingShiftRecord;
import ohd.hseb.raxdb.generated.RaxRatingTable;

public class RatingDifferenceMgr extends BaseDifferenceMgr implements IDifferenceMgr
{

    Map _riverstatGsnoMap = null;

    public RatingDifferenceMgr(RaxSyncDataMgr dm)
    {
        setDataMgr(dm);
        
        setupOutputFiles("Rating");

    } // end of Constructor
    
    //----------------------------------------------------------------------------------------------
    public DiffSet findDifferences()
    {
        DiffSet tableDifferences = new DiffSet();

        List ihfsRecordList = null;
        List ihfsRatingShiftRecordList = null;
        List raxRecordList = null;
        List raxRatingShiftRecordList = null;
        
        RecordDifference oneDifference = null;


        logApplicationMessage("\n" + _dm.getDateTimeStamp() + " Begin Analyzing Rating Table records...");
        String tokenName = "adb_sync_ihfs_units";
        logApplicationMessage("Apps_defaults Token " + tokenName + " set to: " + _dm.getRawTokenValue(tokenName) +
                              "\nValue being used by this application is " + _dm.getRatingCurveUnits());
        tokenName = "adb_sync_ihfs_interpolate";
        logApplicationMessage("Apps_defaults Token " + tokenName + " set to: " + _dm.getRawTokenValue(tokenName) +
                              "\nValue being used by this application is " + _dm.getInterpolateType());
        
        // Organize ALL of the individual IHFS Rating table database records into RaxRatingPoint objects (pairs)
        // Then take all of these pairs (RaxRatingPoints) for one lid and combine then in one RaxRating object (rating cure)

        // get a list of ALL of the individual IHFS Rating table records
        ihfsRecordList = _dm.getIhfsRatingRecordList();

        String previousIhfsLid = "RussErb";
        List listOfRatingPoints = null;
        List listOfIhfsRatingCurves = new ArrayList();
        // Loop through ALL of the IHFS Rating table records
        for (int i=0; i<ihfsRecordList.size(); i++)
        {
            RatingRecord currentIhfsRecord = (RatingRecord) ihfsRecordList.get(i);
          
            // use the current IHFS rating curve pair (stage & discharge) to create a RaxRatingPoint object 
            RaxRatingPoint oneRatingPoint = new RaxRatingPoint(currentIhfsRecord.getStage(), currentIhfsRecord.getDischarge()); 
            String currentIhfsLid = currentIhfsRecord.getLid();

            // lid has changed so create a RaxRating object for this lid
            if(!currentIhfsLid.equals(previousIhfsLid))
            {
                if (listOfRatingPoints != null)
                {
                    RaxRating oneIhfsRatingCurve = new RaxRating(previousIhfsLid, listOfRatingPoints);
                    listOfIhfsRatingCurves.add(oneIhfsRatingCurve);
                    _numberOfIhfsRecords = _numberOfIhfsRecords + 1;
                }
                
                listOfRatingPoints = new ArrayList();
                listOfRatingPoints.add(oneRatingPoint);
            }
            else  // still the same location
            {
                listOfRatingPoints.add(oneRatingPoint);
            }

            // set the previous lid to now be the current IHFS record's lid 
            previousIhfsLid = currentIhfsLid;

        } // for loop of IHFS Rating records
        
        // Finished looping through ALL IHFS Rating records
        // so create a RaxRating object for the last lid
        if (ihfsRecordList.size() > 0)
        {
            RaxRating oneIhfsRatingCurve = new RaxRating(previousIhfsLid, listOfRatingPoints);
            listOfIhfsRatingCurves.add(oneIhfsRatingCurve);
            _numberOfIhfsRecords = _numberOfIhfsRecords + 1;           
        }

        // get a list of ALL of the individual IHFS RatingShift table records
        ihfsRatingShiftRecordList = _dm.getIhfsRatingShiftRecordList();
        // convert them to RaxRatingShiftRecord objects
        // create a Hash Map of IHFS Rax Rating Shift records
        Map ihfsRaxRatingShiftRecordMap = new HashMap();
        for (int i=0; i < ihfsRatingShiftRecordList.size(); i++)
        {
            RaxRatingShiftRecord record = createRaxRatingShiftRecordFromIHFS((RatingShiftRecord) ihfsRatingShiftRecordList.get(i));
            String ratingShiftMapKey = record.getLid();
            ihfsRaxRatingShiftRecordMap.put(ratingShiftMapKey, record);
        } // end of ihfsRatingShiftRecordList for loop
       
        
        // get a list of ALL of the individual RAX RatingShift table records
        raxRatingShiftRecordList = _dm.getRaxRatingShiftRecordList();
        // create a Hash Map of RAX Rating Shift records
        Map raxRaxRatingShiftRecordMap = new HashMap();
        for (int i=0; i < raxRatingShiftRecordList.size(); i++)
        {
            RaxRatingShiftRecord record = (RaxRatingShiftRecord) raxRatingShiftRecordList.get(i);
            String ratingShiftMapKey = record.getLid();
            raxRaxRatingShiftRecordMap.put(ratingShiftMapKey, record);
        } // end of raxRatingShiftRecordList for loop
       
        
        // get a list of ALL of the RAX Rating table records
        raxRecordList = _dm.getRaxRatingRecordList();
        // create Rax Rating Curve from a Rax Rating Record and add to a Hash Map
        Map raxRatingCurveMap = new HashMap();
        for (int i=0; i < raxRecordList.size(); i++)
        {
            RaxRatingRecord record = (RaxRatingRecord) raxRecordList.get(i);
            RaxRating oneRaxRatingCurve = new RaxRating(record);
            String ratingCurveMapKey = oneRaxRatingCurve.getLid();
            raxRatingCurveMap.put(ratingCurveMapKey, oneRaxRatingCurve);
        } // end of raxRecordList for loop
        
        // loop through IHFS rating curve list and check if same key found in RAX
        // if not, this needs to be added as new to RAX
        // if same key found then check to see if they are "different"
        for (int i=0; i<listOfIhfsRatingCurves.size(); i++)
        {
            List fieldDifferenceList = null;

            RaxRating currentIhfsRatingCurve = (RaxRating) listOfIhfsRatingCurves.get(i);
            String currentIhfsMapKey = currentIhfsRatingCurve.getLid();
            
            RaxRatingShiftRecord ihfsRatingShift = (RaxRatingShiftRecord) ihfsRaxRatingShiftRecordMap.get(currentIhfsMapKey);
            RaxRating raxRatingCurve = (RaxRating) raxRatingCurveMap.get(currentIhfsMapKey);
            RaxRatingShiftRecord raxRatingShift = (RaxRatingShiftRecord) raxRaxRatingShiftRecordMap.get(currentIhfsMapKey);

            if (raxRatingCurve == null)  //not in the rax database
            {
                // see if there is an associated "IHFS" Rax Rating Shift record for this IHFS lid
                oneDifference = createDifferenceEntry(RecordDifferenceOriginType.NEW, currentIhfsRatingCurve, ihfsRatingShift, null, null, null);
                tableDifferences.addDifference(oneDifference);
                _numberOfNewRecords++;
            }
            else //match found
            {
                fieldDifferenceList = getFieldDifferenceList(currentIhfsRatingCurve, raxRatingCurve);
                if (fieldDifferenceList.size() == 0) //same
                {
                    _numberOfSameRecords++;
                }
                else //different
                {
                    oneDifference = createDifferenceEntry(RecordDifferenceOriginType.MOD, currentIhfsRatingCurve, ihfsRatingShift, raxRatingCurve, raxRatingShift, fieldDifferenceList);
                    tableDifferences.addDifference(oneDifference);
                    _numberOfModRecords++;
                } 
            }

        } // end of ihfsRecordList for loop
                
        displayEndOfFindDifferencesMessage("Rating");
        
        return tableDifferences;

    } // end of findDifferences method

    //----------------------------------------------------------------------------------------------
    private List getFieldDifferenceList(RaxRating ihfsRatingCurve, RaxRating raxRatingCurve)
    {
        List fieldDifferenceList = new ArrayList();
        List ihfsRatingPointList = null;

        ihfsRatingPointList = ihfsRatingCurve.getRaxRatingPointList();
        for (int i=0; i<ihfsRatingPointList.size(); i++)
        {
            RaxRatingPoint ihfsRatingPointPair = (RaxRatingPoint) ihfsRatingPointList.get(i);
            double ihfsStage = ihfsRatingPointPair.getStage();
            double ihfsDischarge = ihfsRatingPointPair.getDischarge();
            double raxDischarge = raxRatingCurve.getDischarge(ihfsStage);
            
            if (raxDischarge == RaxRating.MISSING)
            {
                raxDischarge = DbTable.getNullDouble();
            }

            checkAndAddToFieldDifferenceList("" + ihfsStage, ihfsDischarge, raxDischarge, fieldDifferenceList);
        }
        
        return fieldDifferenceList;

    } // end of getFieldDifferenceList method
    
    //----------------------------------------------------------------------------------------------
    private RecordDifference createDifferenceEntry(RecordDifferenceOriginType type, RaxRating ihfsRatingCurve, RaxRatingShiftRecord ihfsRatingShift,
            final RaxRating raxRatingCurve, final RaxRatingShiftRecord raxRatingShift, List fieldDiffList)
    {
        RecordDifference oneDifferenceRecord = null;

        RaxRating        newRaxRatingCurve = null;
        RaxRatingShift   newRaxRatingShift = null;
        RaxRatingShift   ihfsRaxRatingShift = null;

        if (ihfsRatingShift != null)
            ihfsRaxRatingShift = new RaxRatingShift(ihfsRatingShift);

        if (type == RecordDifferenceOriginType.NEW)
        {
            // Rating Curve stuff
            newRaxRatingCurve = new RaxRating();

            newRaxRatingCurve.setLid(ihfsRatingCurve.getLid());
            newRaxRatingCurve.setPe("HG");
            newRaxRatingCurve.setTable(1.0);
            newRaxRatingCurve.setValidDate(currentTimeMillisTrimmedToSeconds());
            newRaxRatingCurve.setSrc("IHFS");
            newRaxRatingCurve.setOthagid(getGsnoValue(ihfsRatingCurve.getLid()));
            newRaxRatingCurve.setRfsInput(false);
            newRaxRatingCurve.setRaxRatingPointList(ihfsRatingCurve.getRaxRatingPointList());
            newRaxRatingCurve.setUnits(_dm.getRatingCurveUnits());
            newRaxRatingCurve.setInterpolate(_dm.getInterpolateType());
            newRaxRatingCurve.setOffsets(null);
            newRaxRatingCurve.setAllowStage(DbTable.getNullDouble());
            
            // Rating Shift stuff
            newRaxRatingShift = new RaxRatingShift();

            newRaxRatingShift.setLid(newRaxRatingCurve.getLid());
            newRaxRatingShift.setPe(newRaxRatingCurve.getPe());
            newRaxRatingShift.setRatingTableNumber(newRaxRatingCurve.getTable());
            newRaxRatingShift.setBeginDate(newRaxRatingCurve.getValidDate());
            newRaxRatingShift.setSource(newRaxRatingCurve.getSrc());           

            if (ihfsRatingShift != null)
            {               
                newRaxRatingShift.setValA(-9999.0);
                newRaxRatingShift.setShiftA(ihfsRatingShift.getSh_a());
            }
            else
            {
                newRaxRatingShift.setValA(0.0);
                newRaxRatingShift.setShiftA(0.0);
            }

            newRaxRatingShift.setValB(0.0);
            newRaxRatingShift.setShiftB(0.0);
            newRaxRatingShift.setValC(0.0);
            newRaxRatingShift.setShiftC(0.0);
            newRaxRatingShift.setValD(0.0);
            newRaxRatingShift.setShiftD(0.0);
            newRaxRatingShift.setDatumAdjustment(0.0);

        }
        else if (type == RecordDifferenceOriginType.MOD)
        {

            newRaxRatingCurve = new RaxRating(raxRatingCurve);            

            if (raxRatingShift != null)
            {
                newRaxRatingShift = new RaxRatingShift(raxRatingShift);
            }
            else
            {
                newRaxRatingShift = new RaxRatingShift();
                newRaxRatingShift.setLid(newRaxRatingCurve.getLid());
                newRaxRatingShift.setPe(newRaxRatingCurve.getPe());
                newRaxRatingShift.setRatingTableNumber(newRaxRatingCurve.getTable());
                newRaxRatingShift.setBeginDate(newRaxRatingCurve.getValidDate());
                newRaxRatingShift.setSource(newRaxRatingCurve.getSrc());           
                newRaxRatingShift.setValA(0.0);
                newRaxRatingShift.setShiftA(0.0);
                newRaxRatingShift.setValB(0.0);
                newRaxRatingShift.setShiftB(0.0);
                newRaxRatingShift.setValC(0.0);
                newRaxRatingShift.setShiftC(0.0);
                newRaxRatingShift.setValD(0.0);
                newRaxRatingShift.setShiftD(0.0);
                newRaxRatingShift.setDatumAdjustment(0.0);
            }
            
        }

        // Convert the RaxRating & RaxRatingShift objects into RatingCurveHolder objects which extends the dbRecord class
        // in order to pass them into the RecordDifference constructor
        RatingCurveHolder ihfsRatingCurveHolder = new RatingCurveHolder(ihfsRatingCurve,ihfsRaxRatingShift);
        RatingCurveHolder raxRatingCurveHolder = new RatingCurveHolder(newRaxRatingCurve, newRaxRatingShift);
        
        oneDifferenceRecord = new RecordDifference(type, this, ihfsRatingCurveHolder, raxRatingCurveHolder, fieldDiffList);
        return oneDifferenceRecord;

    } // end of createDifferenceEntry method

    //----------------------------------------------------------------------------------------------
    public void reportDifferences(RecordDifference oneRecordDifference)
    {
        FieldDifference oneFieldDifference = new FieldDifference();

        RatingCurveHolder ihfsRatingCurveHolder = (RatingCurveHolder) oneRecordDifference.getIhfsRecord();
        RatingCurveHolder raxRatingCurveHolder  = (RatingCurveHolder) oneRecordDifference.getRaxRecord();

        RaxRating ihfsRatingCurve = ihfsRatingCurveHolder.getRatingCurve();
        RaxRating raxRatingCurve  = raxRatingCurveHolder.getRatingCurve();
        
        RaxRatingShift ihfsRatingShift = ihfsRatingCurveHolder.getRatingShift();
        RaxRatingShift raxRatingShift  = raxRatingCurveHolder.getRatingShift();
        
        if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
        {
            logRecordDifferenceMessage("\nDifferences found in Table = Rating for key = " + createPrimaryKey(raxRatingCurve));
            for (int j=0; j<oneRecordDifference.getFieldDifferenceList().size(); j++)
            {
                oneFieldDifference = (FieldDifference) oneRecordDifference.getFieldDifferenceList().get(j); 
                logRecordDifferenceMessage("IHFS Stage = " + oneFieldDifference.getName() + "   IHFS_discharge = "
                        + oneFieldDifference.getIhfsValue() + "   RAX_discharge = " + oneFieldDifference.getRaxValue());
            }
        }
        else
        {
            logNewRecordMessage("\nA new Rating record will need to be created in RAX db for key = " + createPrimaryKey(raxRatingCurve) +
                                "\nlid = " + raxRatingCurve.getLid() +
                                "\npe = " + raxRatingCurve.getPe() +
                                "\ntbl = " + displayDouble(raxRatingCurve.getTable()) +
                                "\nvalid_date = " + displayDateTime(raxRatingCurve.getValidDate()) +
                                "\nsrc = " + raxRatingCurve.getSrc() +
                                "\nothagid = " + raxRatingCurve.getOthagid() +
                                "\nrfs_input = " + convertRfsInput(raxRatingCurve.isRfsInput()) +
                                "\nstgflow = " + raxRatingCurve.getRaxRatingPointList() +  
                                "\nunits = " + raxRatingCurve.getUnits() +
                                "\ninterpolate = " + raxRatingCurve.getInterpolate() +
                                "\noffsets = " + raxRatingCurve.getOffsets() +
                                "\nallowstg = " + displayDouble(raxRatingCurve.getAllowStage()));

            logNewRecordMessage("\nA new RatingShift record will need to be created in RAX db for key = " + createPrimaryKey(raxRatingShift) +
                                "\nlid = " + raxRatingShift.getLid() +
                                "\npe = " + raxRatingShift.getPe() +
                                "\ntbl_ver = " + displayDouble(raxRatingShift.getRatingTableNumber()) +
                                "\nbegin_date = " + displayDateTime(raxRatingShift.getBeginDate()) +
                                "\nsrc = " + raxRatingCurve.getSrc() +
                                "\nval_a = " + displayDouble(raxRatingShift.getValA()) +
                                "\nsh_a = " + displayDouble(raxRatingShift.getShiftA()) +
                                "\nval_b = " + displayDouble(raxRatingShift.getValB()) +
                                "\nsh_b = " + displayDouble(raxRatingShift.getShiftB()) +
                                "\nval_c = " + displayDouble(raxRatingShift.getValC()) +
                                "\nsh_c = " + displayDouble(raxRatingShift.getShiftC()) +
                                "\nval_d = " + displayDouble(raxRatingShift.getValD()) +
                                "\nsh_d = " + displayDouble(raxRatingShift.getShiftD()) +
                                "\ndatum_adj = " + displayDouble(raxRatingShift.getDatumAdjustment()));

            logDebugMessage("\nA new Rating record will need to be created in RAX db for key = " + createPrimaryKey(raxRatingCurve) +
                            "\nusing the following information from the IHFS db:" +
                            "\nlid = " + raxRatingCurve.getLid() + "   IHFS_lid = " + ihfsRatingCurve.getLid() +
                            "\npe = " + raxRatingCurve.getPe() +
                            "\ntbl = " + raxRatingCurve.getTable() +
                            "\nvalid_date = " + displayDateTime(raxRatingCurve.getValidDate()) +
                            "\nsrc = " + raxRatingCurve.getSrc() +
                            "\nothagid = " + raxRatingCurve.getOthagid() +
                            "\nrfs_input = " + convertRfsInput(raxRatingCurve.isRfsInput()) +
                            "\nstgflow      = " + raxRatingCurve.getRaxRatingPointList() +       
                            "\nIHFS_stgflow = " + ihfsRatingCurve.getRaxRatingPointList() +          
                            "\nunits = " + raxRatingCurve.getUnits() +
                            "\ninterpolate = " + raxRatingCurve.getInterpolate() +
                            "\noffsets = " + raxRatingCurve.getOffsets() +
                            "\nallowstg = " + displayDouble(raxRatingCurve.getAllowStage()));

            logDebugMessage("\nA new RatingShift record will need to be created in RAX db for key = " + createPrimaryKey(raxRatingShift));
            if (ihfsRatingShift == null)
                logDebugMessage("No RatingShift Records found in the IHFS database for this lid");
            else
            {
                logDebugMessage("IHFS lid = " + ihfsRatingShift.getLid() +
                        "   date = " + displayDateTime(ihfsRatingShift.getBeginDate()) +
                        "   shift_amount = " + displayDouble(ihfsRatingShift.getShiftA()));
            }
            logDebugMessage("lid = " + raxRatingShift.getLid() +
                            "\npe = " + raxRatingShift.getPe() +
                            "\ntbl_ver = " + displayDouble(raxRatingShift.getRatingTableNumber()) +
                            "\nbegin_date = " + displayDateTime(raxRatingShift.getBeginDate()) +
                            "\nsrc = " + raxRatingShift.getSource() +
                            "\nval_a = " + displayDouble(raxRatingShift.getValA()) +
                            "\nsh_a = " + displayDouble(raxRatingShift.getShiftA()) +
                            "\nval_b = " + displayDouble(raxRatingShift.getValB()) +
                            "\nsh_b = " + displayDouble(raxRatingShift.getShiftB()) +
                            "\nval_c = " + displayDouble(raxRatingShift.getValC()) +
                            "\nsh_c = " + displayDouble(raxRatingShift.getShiftC()) +
                            "\nval_d = " + displayDouble(raxRatingShift.getValD()) +
                            "\nsh_d = " + displayDouble(raxRatingShift.getShiftD()) +
                            "\ndatum_adj = " + displayDouble(raxRatingShift.getDatumAdjustment()));
        }

    } // end of reportDifferences method

    //----------------------------------------------------------------------------------------------
    private String createPrimaryKey(RaxRating rating)
    {
        String key = null;
        
        key = rating.getLid() + "|" + rating.getPe() + "|" + rating.getTable() + "|" + displayDateTime(rating.getValidDate()) +"|" + rating.getSrc() +"|";
        
        return key;
    }
    //----------------------------------------------------------------------------------------------
    private String createPrimaryKey(RaxRatingShift ratingShift)
    {
        String key = null;
        
        key = ratingShift.getLid() + "|" + ratingShift.getPe() + "|" + ratingShift.getRatingTableNumber() + "|"
                                         + displayDateTime(ratingShift.getBeginDate()) +"|" + ratingShift.getSource() +"|";
        
        return key;
    }

    //----------------------------------------------------------------------------------------------
    private String convertRfsInput(final boolean rfsInputValue)
    {
        String returnValue = null;

        if (rfsInputValue)
        {
            returnValue = "Y";
        }
        else
        {
            returnValue = "N"; 
        }

        return returnValue;

    }  // end of convertRfsInput method
    
    //----------------------------------------------------------------------------------------------
    private RaxRatingShiftRecord createRaxRatingShiftRecordFromIHFS(RatingShiftRecord ihfsRatingShift)
    {
        RaxRatingShiftRecord raxRatingShift = new RaxRatingShiftRecord();
        
        if (ihfsRatingShift != null)
        {
            raxRatingShift.setLid(ihfsRatingShift.getLid());
            raxRatingShift.setBegin_date(ihfsRatingShift.getDate());
            raxRatingShift.setSh_a(ihfsRatingShift.getShift_amount());
        }
        
        return raxRatingShift;
    }
    
    //----------------------------------------------------------------------------------------------
    private String getGsnoValue(String lid)
    {
        if (_riverstatGsnoMap == null)
        {
            List riverstatRecordList = _dm.getIhfsRiverstatRecordList();

            _riverstatGsnoMap = new HashMap();
            //add the gsno column to a Map
            for (int i=0; i < riverstatRecordList.size(); i++)
            {
                RiverstatRecord record = (RiverstatRecord) riverstatRecordList.get(i);
                _riverstatGsnoMap.put(record.getLid(), record.getGsno());
            }
        }

        String gsnoValue = (String) _riverstatGsnoMap.get(lid);

        return gsnoValue;
    }
    
    //----------------------------------------------------------------------------------------------
    public int processDifferences(RecordDifference oneRecordDifference)
    {
        int numberOfFailedDbUpdates = 0;
        
        // process the differences for the Rating table ONLY
        numberOfFailedDbUpdates = processRatingDifferences(oneRecordDifference);

        // create an object of RatingShift Difference Manager and call its process difference method
        RatingShiftDifferenceMgr ratingShiftDifferenceManager = new RatingShiftDifferenceMgr(_dm);       
        // process the differences for the RatingShift table ONLY
        numberOfFailedDbUpdates += ratingShiftDifferenceManager.processDifferences(oneRecordDifference);
        
        return numberOfFailedDbUpdates;

    } // end of processDifferences method

    //----------------------------------------------------------------------------------------------
    private int processRatingDifferences(RecordDifference oneRecordDifference)
    {
        int numberOfFailedDbUpdates = 0;

        RatingCurveHolder raxRatingCurveHolder  = (RatingCurveHolder) oneRecordDifference.getRaxRecord();
        RaxRating raxRatingCurve  = raxRatingCurveHolder.getRatingCurve();
        
        RaxRatingRecord raxRatingRecord  = RaxRating.getRaxRatingRecord(raxRatingCurve);

        RaxRatingTable raxRatingTable = _dm.getRaxRatingTable();

        if (oneRecordDifference.getDiffType() == RecordDifferenceOriginType.MOD)
        {
            RatingCurveHolder ihfsRatingCurveHolder = (RatingCurveHolder) oneRecordDifference.getIhfsRecord();
            RaxRating ihfsRatingCurve = ihfsRatingCurveHolder.getRatingCurve();
            List ihfsRatingPointList = ihfsRatingCurve.getRaxRatingPointList();
            for (int i=0; i<ihfsRatingPointList.size(); i++)
            {
                RaxRatingPoint ihfsRatingPointPair = (RaxRatingPoint) ihfsRatingPointList.get(i);
                raxRatingCurve.addRatingPoint(ihfsRatingPointPair);
            }
            
            raxRatingCurve.setValidDate(currentTimeMillisTrimmedToSeconds());

            RaxRatingRecord newRaxRecord =  RaxRating.getRaxRatingRecord(raxRatingCurve);

            numberOfFailedDbUpdates = processRaxInsertUpdate(INSERT, "Rating", raxRatingTable, newRaxRecord, null);
        }
        else
        {
            numberOfFailedDbUpdates = processRaxInsertUpdate(INSERT, "Rating", raxRatingTable, raxRatingRecord, null);
        }
        
        return numberOfFailedDbUpdates;

    } // end of processRatingDifferences method

    //----------------------------------------------------------------------------------------------
    protected String formatColumnsForInsertUpdateDisplay(DbRecord dbRecord)
    {
        RaxRatingRecord raxRecord = (RaxRatingRecord) dbRecord;

        String formatedString = new String("|" + raxRecord.getLid() + "|"
                + raxRecord.getPe1() + "|"
                + raxRecord.getPe2() + "|"
                + displayDouble(raxRecord.getTbl()) + "|"
                + displayDateTime(raxRecord.getValid_date()) + "|"
                + raxRecord.getSrc() + "|"
                + raxRecord.getOthagid() + "|"
                + raxRecord.getRfs_input() + "|"
                + raxRecord.getStgflow() + "|"
                + raxRecord.getUnits() + "|"
                + raxRecord.getInterpolate() + "|"
                + raxRecord.getOffsets() + "|"
                + displayDouble(raxRecord.getAllowstg()) + "|");

        return formatedString;

    } // end formatColumnsForInsertUpdateDisplay method

    //----------------------------------------------------------------------------------------------
    protected int insert(DbTable dbTable, DbRecord dbRecord) throws SQLException
    {
        RaxRatingTable table = (RaxRatingTable) dbTable;
        RaxRatingRecord record = (RaxRatingRecord) dbRecord;
        
        return table.insert(record);
    }

    //----------------------------------------------------------------------------------------------
    protected int update(DbTable dbTable, DbRecord dbRecordOld, DbRecord dbRecordNew) throws SQLException
    {
        RaxRatingTable table = (RaxRatingTable) dbTable;
        RaxRatingRecord recordOld = (RaxRatingRecord) dbRecordOld;
        RaxRatingRecord recordNew = (RaxRatingRecord) dbRecordNew;
        
        return table.update(recordOld, recordNew);
    }

    //==============================================================================================
    // The code BELOW is for TESTING ONLY
    
    public void testRating()
    {
        
        String origBluo2ArrayString = "{{1,2.0},{1.5,3.0},{2,4}}";
        String newBluo2ArrayString = "{{1,2.0},{3.0,4.0},{5,6}}";
     
        try
        {
            CustomRaxRatingTable table = (CustomRaxRatingTable) _dm.getRaxRatingTable();
       //    new3sizeArrayString
            
            //update 1 record
            String newLid = "CHIPT2";
            String where = "where lid = '" + newLid +"' ";
            String allWhere = "order by lid ";
            List recordList = table.selectNRecords(allWhere, 1);
            
            if (recordList.size() > 0)
            {
                RaxRatingRecord record = (RaxRatingRecord) recordList.get(0);
                System.out.println("In Database, record = " + record);
                
                RaxRatingRecord newRecord = new RaxRatingRecord(record);
                newRecord.setLid(newLid);

                
                //delete test record
                table.delete(where);

                
                //insert a copy of the first retrieved record, with the lid changed
                table.insert(newRecord);
                System.out.print("After insert: ");
                selectAndPrint(table, where);
                
                
                //update
                RaxRatingRecord newerRecord = new RaxRatingRecord(newRecord);
                newerRecord.setSrc("CHIP");
                newerRecord.setStgflow(null);
                table.update(newRecord, newerRecord);
                
                System.out.print("After update: ");
                selectAndPrint(table, where);
                
                System.out.println(" ----" +  record);
            }
      /*    
            //print all records
         //   recordList = table.selectNRecords("order by lid ", 150);
            recordList = table.selectNRecords(bluo2Where, 150);
            
            
            for (int i=0; i<recordList.size(); i++)
            {
                RatingRecord record = (RatingRecord) recordList.get(i);             
                System.out.println(record);
            }
        */
        }
        catch (SQLException e)
        {
            e.printStackTrace();
        }

    }
    
   
    private void selectAndPrint(CustomRaxRatingTable table, String whereString)
    {
        String header = "RatingDifferenceMgr.selectAndPrint(): ";
        try
        {
            
           System.out.println(header + "whereString = " + whereString + "\n Results = ");
           List<RaxRatingRecord> recordList = table.select(whereString);
           
           
           int i = 0;
           for (RaxRatingRecord record : recordList)
           {
               System.out.println(record);
               i++;
           }
           
        }
        catch (SQLException e)
        {
       
        }
    } 

    public static void main(String[] argArray)
    {
        String ihfsConnectionString = "jdbc:postgresql://lx5:5432/hd_ob83fwr?user=pguser";
        String raxConnectionString = "jdbc:postgresql://lx5:5432/adb_ob83raxtest?user=pguser";
        RaxSyncDataMgr dataMgr = new RaxSyncDataMgr(ihfsConnectionString, raxConnectionString );
        RatingDifferenceMgr diffMgr = new RatingDifferenceMgr(dataMgr);
        
        diffMgr.testRating();
    }
    //==============================================================================================

} // end of RatingDifferenceMgr class
