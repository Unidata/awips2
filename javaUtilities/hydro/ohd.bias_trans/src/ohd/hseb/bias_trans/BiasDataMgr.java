package ohd.hseb.bias_trans;


import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.sql.*;
import java.util.*;
import ohd.hseb.db.*;
import ohd.hseb.ihfsdb.generated.*;
import ohd.hseb.util.AppsDefaults;

/**
 * 
 * This class is the bridge between the database and the radar bias message for the BiasMessageCreator
 * and the BiasMessageReader programs.
 * @author lawrence
 *
 */

public class BiasDataMgr implements RfcBiasConstants
{
   private Database _db;
   
   //private DataOutputStream _outFile;
   private File _file;

   private String _fxaLocalSite;
   private String _jdbcUrl;
   private String _rfcId;
   private String _fileName;
   private String _outputFilePath;
   
   private final String RFC_BIAS_MESSAGE_DIR_TOK = "rfc_bias_output_dir";
   private final String FXA_LOCAL_SITE_TOK = "FXA_LOCAL_SITE";
   
   private List _radarIdList = null;
   
   
   public BiasDataMgr ( String jdbcUrl ) throws Exception
   {
     _jdbcUrl = jdbcUrl;
     
     _db = new Database ( );
     
          // Create a database connection.
     _db.connectWithDriverSearch( _jdbcUrl );
     
     // Get Tokens needed for queries and to build the file path.
     getBiasTokens();

   }

   
   public RadarBias getRadarBias ( String dateTime ) throws Exception
   {
       RadarBias radarBias = new RadarBias();
       radarBias.setDateTime(dateTime);
       radarBias.setMemorySpanGroup ( getMemorySpanGroup() );
       List radarIdList = getRadarIdList ( );
       radarBias.setBiasDynGroupList ( getBiasDynGroupList ( radarIdList, dateTime ) );
       return radarBias;
   }
   
   private DataOutputStream openOutputFile ( String dateTime ) throws FileNotFoundException
   {
       DataOutputStream outFile = null;
       
       // Create the file path
       _fileName = _rfcId + dateTime + "z"; 
       File file = new File ( _outputFilePath, _fileName);
       
       // Open the bias message file.
       try
       {
          outFile = new DataOutputStream ( new BufferedOutputStream
                                 (new FileOutputStream( file)));
       }
       catch ( FileNotFoundException e)
       {
           System.out.println ( "Could not open file " + file.toString () );
           throw e;
       }
       
       System.out.println ( "Opened file " + file.toString() );
       
       return outFile;
   }
   
   private DataInputStream openInputFile ( File filePath ) throws FileNotFoundException
   {
       DataInputStream inputStream = null;
       
       try
       {
           _file = filePath;
           inputStream = new DataInputStream ( new BufferedInputStream
                                        ( new FileInputStream ( filePath)));
       }
       catch ( FileNotFoundException e)
       {
           System.out.println ( "Could not open file " + filePath.toString() );
           throw e;
       }
       
       System.out.println ( "Opened file " + filePath.toString ( ) );
       return inputStream;
   }
   
   private void getBiasTokens ( ) throws Exception
   {
       AppsDefaults apps_defaults;
       String token;
       
       apps_defaults = new AppsDefaults ( );
       token = apps_defaults.getToken ( RFC_BIAS_MESSAGE_DIR_TOK );
       
       if ( token == null || token.length() == 0 )
       {
           String message = "Token " + RFC_BIAS_MESSAGE_DIR_TOK + " not defined.";
           throw new Exception( message );
       }
       
       _outputFilePath = token;
       
       token = apps_defaults.getToken( FXA_LOCAL_SITE_TOK );
       
       if ( token == null || token.length() == 0 )
       {
           String message = "Token " + FXA_LOCAL_SITE_TOK + " not defined.";
           throw new Exception(message);
       }
       
       _rfcId = token.toUpperCase();
       _fxaLocalSite = _rfcId;
   }
   
   private MemorySpanGroup getMemorySpanGroup ( ) throws Exception
   {
       List biasStatList = null;
       RWBiasStatTable biasStat = new RWBiasStatTable( _db );
       RWBiasStatRecord record = null;
       MemorySpanGroup rwBiasStat = null;

       try 
       {
           biasStatList = biasStat.select ("WHERE office_id = '" + _fxaLocalSite + "'");

           if ( !biasStatList.isEmpty())
           {
               record = (RWBiasStatRecord) biasStatList.get(0);
               rwBiasStat = getRWBiasStatFromRecord(record);
           }
           else
           {
               throw new Exception ("The RWBiasStat table is empty.");
           }
       }
       catch ( SQLException e )
       {
           System.out.println( e.getMessage());
           throw e;
       }         
     
       return rwBiasStat;
   }
   
   private List loadRadarIdList ( )
   {
      // List recordList = null;;
       RadarLocTable table = new RadarLocTable ( _db );
       List radarIdList = new ArrayList();

       try
       {
           List<RadarLocRecord> recordList = table.select("WHERE use_radar = 'T' ORDER BY radid ASC");

       //   Iterator iterator = recordList.iterator();

         //  while ( iterator.hasNext())
           for (RadarLocRecord record : recordList )
           {
               //RadarLocRecord record = (RadarLocRecord) iterator.next();
               radarIdList.add(record.getRadid());
           }
       }
       catch ( SQLException e)
       {
           System.out.println(e.getMessage());
           return null;
       }
       
       if ( radarIdList.size() == 0 )
       {
           System.out.println ( "No radars found in the RadarLoc table.");
       }

       return radarIdList;
   }
   
   public Set loadRadarIdSetForGivenOffice ( )
   {
       List recordList = null;
       RadarLocTable table = new RadarLocTable ( _db );
       Set radarIdSet = new HashSet();

       try
       {
           recordList = table.select("WHERE use_radar = 'T' AND office_id = '" + _rfcId +
                                     "' ORDER BY radid ASC");

           Iterator iterator = recordList.iterator();

           while ( iterator.hasNext())
           {
               RadarLocRecord record = (RadarLocRecord) iterator.next();
               radarIdSet.add(record.getRadid());
           }
       }
       catch ( SQLException e)
       {
           System.out.println(e.getMessage());
           return null;
       }
       
       if ( radarIdSet.size() == 0 )
       {
           System.out.println ("No radars found for " + _rfcId + " in RadarLoc Table.");
       }

       return radarIdSet;
   }
   
   // ----------------------------------------------------------------------------------
   private List getBiasDynGroupList ( List radarIdList, String _dateTime )
   {
       Iterator biasDynIterator = null;
       Iterator radarIdIterator = null;
       List recordList = null;
       List biasDynGroupList = new ArrayList ();
       RWBiasDynRecord record;
       RWBiasDynTable table = new RWBiasDynTable ( _db );
       String obsTime;
       String radarId;
       
       /* Format the obstime as YYYY-MM-DD HH:MM:SS for use in the RWBiasDyn
        * query.
        */
       obsTime = _dateTime.substring(0, NUM_DIGITS_IN_YEAR) + "-" +
                 _dateTime.substring(NUM_DIGITS_IN_YEAR,NUM_DIGITS_IN_YEAR +
                                                        NUM_DIGITS_IN_MONTH ) + "-" +
                 _dateTime.substring(NUM_DIGITS_IN_YEAR + NUM_DIGITS_IN_MONTH,
                                     NUM_DIGITS_IN_YEAR + NUM_DIGITS_IN_MONTH +
                                     NUM_DIGITS_IN_DAY) + " " +
                 _dateTime.substring(NUM_DIGITS_IN_YEAR + NUM_DIGITS_IN_MONTH +
                                     NUM_DIGITS_IN_DAY, NUM_DIGITS_IN_YEAR + 
                                                        NUM_DIGITS_IN_MONTH +
                                                        NUM_DIGITS_IN_DAY +
                                                        NUM_DIGITS_IN_HOUR ) + ":00:00";                 
                 
       radarIdIterator = radarIdList.iterator();

       while ( radarIdIterator.hasNext())
       {
           List biasDynSetList = new ArrayList();
           radarId = (String) radarIdIterator.next();

           try
           {
               recordList = table.select ("WHERE radid='" + radarId + "' AND " +
                       " obstime = '" + obsTime + "' AND office_id = '" + _fxaLocalSite + 
                       "' ORDER BY " + "memspan_ind ASC");

               biasDynIterator = recordList.iterator();

               while ( biasDynIterator.hasNext())
               {
                   record = (RWBiasDynRecord)biasDynIterator.next();     
                   biasDynSetList.add( getRWBiasDynFromRecord( record ));
               }
               
           }
           catch ( SQLException e)
           {
               System.out.println (e.getMessage());
               return null;
           }
           
           BiasDynGroup biasDynGroup = new BiasDynGroup();
           biasDynGroup.setRadarId(radarId);
           biasDynGroup.setBiasDynSetList(biasDynSetList);
           biasDynGroupList.add(biasDynGroup);
       }

       return biasDynGroupList;
   }

   public List getRadarIdList()
   {
       if ( _radarIdList == null )
       {
           _radarIdList = loadRadarIdList();
       }

       return _radarIdList;
   }

   private MemorySpanGroup getRWBiasStatFromRecord ( RWBiasStatRecord record)
   {
       MemorySpanGroup rwBiasStat = new MemorySpanGroup();
       
       rwBiasStat.setMinGrValueBias(record.getMin_gr_value_bias());
       rwBiasStat.setNpairBiasSelect(record.getNpair_bias_select());
       rwBiasStat.setNpairSvarUpdate(record.getNpair_svar_update());
       rwBiasStat.setNumSpan(record.getNum_span());
       rwBiasStat.setStdCut(record.getStd_cut());
       rwBiasStat.setBiasQcOpt(record.getBias_qc_opt());
       rwBiasStat.setInitSpan(record.getInit_span());
       rwBiasStat.setLagCut(record.getLag_cut());
       rwBiasStat.setMemSpan1(record.getMem_span1());
       rwBiasStat.setMemSpan2(record.getMem_span2());
       rwBiasStat.setMemSpan3(record.getMem_span3());
       rwBiasStat.setMemSpan4(record.getMem_span4());
       rwBiasStat.setMemSpan5(record.getMem_span5());
       rwBiasStat.setMemSpan6(record.getMem_span6());
       rwBiasStat.setMemSpan7(record.getMem_span7());
       rwBiasStat.setMemSpan8(record.getMem_span8());
       rwBiasStat.setMemSpan9(record.getMem_span9());
       rwBiasStat.setMemSpan10(record.getMem_span10());
       
       return rwBiasStat;
   }
   
   private BiasDynSet getRWBiasDynFromRecord ( RWBiasDynRecord record )
   {
       BiasDynSet biasDynSet = new BiasDynSet();
       
       biasDynSet.setMeanFieldBias(record.getBias());
       biasDynSet.setMemSpanIndex(record.getMemspan_ind());
       biasDynSet.setGageRadarPairCount(record.getNumpairs());
       biasDynSet.setSumOfGageValues(record.getSumgag());
       biasDynSet.setSumOfRadarValues(record.getSumrad());
       
       return biasDynSet;
   }
   
   public void writeRadarBiasToFile ( RadarBias radarBias, String dateTime ) throws Exception
   {
       //Open the file
       DataOutputStream outFile = openOutputFile( dateTime );

       //Write Header
       writeHeader ( radarBias, dateTime, outFile );
       
       //Write body
       writeBody ( radarBias, outFile );
       
       //Close File
       outFile.close();
   }

   private void writeHeader ( RadarBias radarBias, String dateTime, 
                              DataOutputStream outFile) throws IOException
   {
       outFile.writeUTF("BIAS");
       outFile.writeUTF(_rfcId);
       outFile.writeUTF(dateTime);
       MemorySpanGroup memorySpanGroup = radarBias.getMemorySpanGroup();
       outFile.writeFloat(memorySpanGroup.getMinGrValueBias());
       outFile.writeInt(memorySpanGroup.getNpairBiasSelect());
       outFile.writeInt(memorySpanGroup.getNpairSvarUpdate());
       outFile.writeInt(memorySpanGroup.getStdCut());
       outFile.writeInt(memorySpanGroup.getLagCut());
       outFile.writeInt(memorySpanGroup.getInitSpan());
       outFile.writeInt(memorySpanGroup.getBiasQcOpt());
       outFile.writeInt(memorySpanGroup.getNumSpan());
       outFile.writeFloat(memorySpanGroup.getMemSpan1());
       outFile.writeFloat(memorySpanGroup.getMemSpan2());
       outFile.writeFloat(memorySpanGroup.getMemSpan3());
       outFile.writeFloat(memorySpanGroup.getMemSpan4());
       outFile.writeFloat(memorySpanGroup.getMemSpan5());
       outFile.writeFloat(memorySpanGroup.getMemSpan6());
       outFile.writeFloat(memorySpanGroup.getMemSpan7());
       outFile.writeFloat(memorySpanGroup.getMemSpan8());
       outFile.writeFloat(memorySpanGroup.getMemSpan9());
       outFile.writeFloat(memorySpanGroup.getMemSpan10());
   }
   
   private void writeBody ( RadarBias radarBias,
                            DataOutputStream outFile) throws IOException
   {
       List biasDynGroupList = radarBias.getBiasDynGroupList();
       
       for ( int i = 0; i < biasDynGroupList.size(); ++i )
       {
           BiasDynGroup biasDynGroup = (BiasDynGroup) biasDynGroupList.get(i);
           outFile.writeUTF ( biasDynGroup.getRadarId());
                     
           List biasDynSetList = biasDynGroup.getBiasDynSetList();
           
           outFile.writeInt(biasDynSetList.size());
           
           for ( int j = 0; j < biasDynSetList.size(); ++j)
           {
               BiasDynSet biasDynSet = (BiasDynSet) biasDynSetList.get(j);
               
               outFile.writeShort(biasDynSet.getMemSpanIndex());
               outFile.writeDouble(biasDynSet.getGageRadarPairCount());
               outFile.writeFloat(biasDynSet.getSumOfGageValues());
               outFile.writeFloat(biasDynSet.getSumOfRadarValues());
               outFile.writeFloat(biasDynSet.getMeanFieldBias());
           }
                      
       }
       
   }
   
   private void readHeader ( DataInputStream inputStream, RadarBias radarBias ) throws IOException
   {
       String biasIndicator;
    
       try
       {
           biasIndicator = inputStream.readUTF();

           if ( biasIndicator.compareTo("BIAS") != 0)
           {
               throw new IOException ( "Invalid bias message indicator field " + biasIndicator);
           }

           _rfcId = inputStream.readUTF();

           /* If, for some reason, this message originated from this office
            * do not process any further. 
            */
           System.out.println ("RFC ID " + _rfcId + " FXA_LOCAL_SITE " + _fxaLocalSite );

           if ( _rfcId.compareTo(_fxaLocalSite) == 0)
           {
               throw new IOException ( _file.getPath() + " originated from this office - " +
                       _fxaLocalSite + "\nThis file will not be processed." );
           }

           radarBias.setDateTime(inputStream.readUTF());

           MemorySpanGroup memorySpanGroup = new MemorySpanGroup();

           memorySpanGroup.setMinGrValueBias(inputStream.readFloat());
           memorySpanGroup.setNpairBiasSelect(inputStream.readInt());
           memorySpanGroup.setNpairSvarUpdate(inputStream.readInt());
           memorySpanGroup.setStdCut(inputStream.readInt());
           memorySpanGroup.setLagCut(inputStream.readInt());
           memorySpanGroup.setInitSpan(inputStream.readInt());
           memorySpanGroup.setBiasQcOpt(inputStream.readInt());
           memorySpanGroup.setNumSpan(inputStream.readInt());
           memorySpanGroup.setMemSpan1(inputStream.readFloat());
           memorySpanGroup.setMemSpan2(inputStream.readFloat());
           memorySpanGroup.setMemSpan3(inputStream.readFloat());
           memorySpanGroup.setMemSpan4(inputStream.readFloat());
           memorySpanGroup.setMemSpan5(inputStream.readFloat());
           memorySpanGroup.setMemSpan6(inputStream.readFloat());
           memorySpanGroup.setMemSpan7(inputStream.readFloat());
           memorySpanGroup.setMemSpan8(inputStream.readFloat());
           memorySpanGroup.setMemSpan9(inputStream.readFloat());
           memorySpanGroup.setMemSpan10(inputStream.readFloat());

           radarBias.setMemorySpanGroup(memorySpanGroup);
       }
       catch (IOException e)
       {
           e.printStackTrace();
           throw e;
       }
   }
   
   /**
   @param inputStream
   @param radarBias
   */
   private void readBody ( DataInputStream inputStream, RadarBias radarBias ) throws IOException
   {
       boolean completeGroup = true;
       int biasSetSize;
       
       /*List biasDynGroupList = radarBias.getBiasDynGroupList();*/
       ArrayList biasDynGroupList = new ArrayList();
       
       try
       {
           while ( true )
           {
               BiasDynGroup biasDynGroup = new BiasDynGroup();

               /* Read a Radar Id.  Are there any left? */
               biasDynGroup.setRadarId(inputStream.readUTF());
               completeGroup = false;
               biasSetSize = inputStream.readInt();
               ArrayList biasDynSetList = new ArrayList();

               for ( int i = 0; i < biasSetSize; ++i )
               {
                   BiasDynSet biasDynSet = new BiasDynSet();
                   biasDynSet.setMemSpanIndex(inputStream.readShort());
                   biasDynSet.setGageRadarPairCount(inputStream.readDouble());
                   biasDynSet.setSumOfGageValues(inputStream.readFloat());
                   biasDynSet.setSumOfRadarValues(inputStream.readFloat());
                   biasDynSet.setMeanFieldBias(inputStream.readFloat());

                   biasDynSetList.add(biasDynSet);
               }

               completeGroup = true;
               biasDynGroup.setBiasDynSetList(biasDynSetList);
               biasDynGroupList.add(biasDynGroup);
           }
       }
       catch (EOFException e)
       {
           if ( !completeGroup )
           {
               throw new IOException ( "Error reading bias message file");
           }
           
       }
       
       radarBias.setBiasDynGroupList(biasDynGroupList);
   }
   
   public RadarBias readRadarBiasFromFile ( File filePath ) throws Exception
   {
       RadarBias radarBias = new RadarBias ();
       
       // Attempt to open the input file.
       DataInputStream inputStream = openInputFile(filePath);
       
       // Read the file header.
       readHeader ( inputStream, radarBias );
       
       // Read the file body.
       readBody ( inputStream, radarBias );
       
       inputStream.close();

       return radarBias;
   }
   
   public void writeRadarBiasToDatabase ( RadarBias radarBias ) throws Exception
   {
       /* Retrieve the list of offices for which to process bias information. */
       Set radarIdSet = loadRadarIdSetForGivenOffice();

       if ( radarIdSet.size() != 0 )
       {

           /* Write to the RWBiasStat table. */
           writeMemorySpanGroup ( radarBias );

           /* Write to the RWBiasDyn table. */
           writeBiasDynSet ( radarBias, radarIdSet );
       }
       else
       {
           System.out.println ( "No radars to process for office " + _rfcId + ".");    
       }
   }
   
   private void writeMemorySpanGroup ( RadarBias radarBias ) throws Exception
   {
       /* The memory span data from the RadarBias object to the 
        * RWBiasStat Record.
        */ 
       RWBiasStatRecord record = null;
       
       try
       {
           RWBiasStatTable table = new RWBiasStatTable ( _db );
           record = getRecordFromMemorySpan ( radarBias.getMemorySpanGroup());
           table.insertOrUpdate(record);
       }
       catch (SQLException e)
       {
           System.out.println("Could not insert/update record in RWBiasStat table.");
           throw e;
       }
   }

   private void writeBiasDynSet ( RadarBias radarBias,
                                 Set radarIdSet ) throws Exception
   {
       RWBiasDynRecord record = null;
       List biasDynGroupList = radarBias.getBiasDynGroupList();
       List biasDynSetList = null;
       BiasDynSet biasDynSet = null;

       BiasDynGroup biasDynGroup = null;

       try
       {
           RWBiasDynTable table = new RWBiasDynTable ( _db );

           /* Construct the obstime into the proper format. */
           String obstimeYYYYMMDD = radarBias.getDateTime();
           String obstimeSQL =  obstimeYYYYMMDD.substring( 0, NUM_DIGITS_IN_YEAR) + "-" +
           obstimeYYYYMMDD.substring( NUM_DIGITS_IN_YEAR,NUM_DIGITS_IN_YEAR +
                   NUM_DIGITS_IN_MONTH ) + "-" +
                   obstimeYYYYMMDD.substring( NUM_DIGITS_IN_YEAR + NUM_DIGITS_IN_MONTH,
                           NUM_DIGITS_IN_YEAR + NUM_DIGITS_IN_MONTH +
                           NUM_DIGITS_IN_DAY) + " " +
                           obstimeYYYYMMDD.substring( NUM_DIGITS_IN_YEAR + NUM_DIGITS_IN_MONTH +
                                   NUM_DIGITS_IN_DAY, NUM_DIGITS_IN_YEAR + 
                                   NUM_DIGITS_IN_MONTH +
                                   NUM_DIGITS_IN_DAY +
                                   NUM_DIGITS_IN_HOUR ) + ":00:00"; 

           for ( int i = 0; i < biasDynGroupList.size(); ++i )
           {
               biasDynGroup = (BiasDynGroup) biasDynGroupList.get(i);

               /* Check if this BiasDyn group radar is in the radar map. */
               if ( radarIdSet.contains(biasDynGroup.getRadarId()))
               {    
                   biasDynSetList = biasDynGroup.getBiasDynSetList();

                   for ( int j = 0; j < biasDynSetList.size(); ++j )
                   {
                       biasDynSet = (BiasDynSet) biasDynSetList.get(j);
                       record = getRecordFromBiasDynSet (biasDynGroup.getRadarId(), 
                               obstimeSQL, biasDynSet);
                       table.insertOrUpdate(record);
                   }
               }
               else
               {
                   System.out.println ( "Bias information from office " + _rfcId + 
                                        " not stored for Radar " + biasDynGroup.getRadarId() + 
                                        ".");
               }
           }
       }
       catch ( SQLException e)
       {
           System.out.println ( "Could not insert/update the RWBiasDynTable.");
           throw e;
       }
   }
   
   private RWBiasStatRecord getRecordFromMemorySpan ( MemorySpanGroup memorySpanGroup )
   {
       RWBiasStatRecord record = new RWBiasStatRecord ( );
       
       record.setBias_qc_opt(memorySpanGroup.getBiasQcOpt());
       record.setInit_span(memorySpanGroup.getInitSpan());
       record.setLag_cut(memorySpanGroup.getLagCut());
       record.setMem_span1(memorySpanGroup.getMemSpan1());
       record.setMem_span2(memorySpanGroup.getMemSpan2());
       record.setMem_span3(memorySpanGroup.getMemSpan3());
       record.setMem_span4(memorySpanGroup.getMemSpan4());
       record.setMem_span5(memorySpanGroup.getMemSpan5());
       record.setMem_span6(memorySpanGroup.getMemSpan6());
       record.setMem_span7(memorySpanGroup.getMemSpan7());
       record.setMem_span8(memorySpanGroup.getMemSpan8());
       record.setMem_span9(memorySpanGroup.getMemSpan9());
       record.setMem_span10(memorySpanGroup.getMemSpan10());
       record.setMin_gr_value_bias(memorySpanGroup.getMinGrValueBias());
       record.setNpair_bias_select(memorySpanGroup.getNpairBiasSelect());
       record.setNpair_svar_update(memorySpanGroup.getNpairSvarUpdate());
       record.setNum_span(memorySpanGroup.getNumSpan());
       record.setStd_cut(memorySpanGroup.getStdCut());
       record.setOffice_id(_rfcId);
       
       return record;
   }
   
   private RWBiasDynRecord getRecordFromBiasDynSet ( String radarId,
                                                     String obstimeSQL,
                                                     BiasDynSet biasDynSet )
   {
       RWBiasDynRecord record = new RWBiasDynRecord();
       
       record.setBias(biasDynSet.getMeanFieldBias());
       record.setMemspan_ind(biasDynSet.getMemSpanIndex());
       record.setNumpairs(biasDynSet.getGageRadarPairCount());
       record.setObstime(DbTimeHelper.getLongTimeFromDateTimeString(obstimeSQL));
       record.setRadid(radarId);
       record.setSumgag(biasDynSet.getSumOfGageValues());
       record.setSumrad(biasDynSet.getSumOfRadarValues());
       record.setOffice_id(_rfcId);
       
       return record;
   }
}




