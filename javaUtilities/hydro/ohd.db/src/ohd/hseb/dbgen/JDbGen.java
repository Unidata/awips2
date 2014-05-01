//
//  File:  JDbGen.java
//  Orig Author: Russell Erb 12/18/2000
//  Modified by: Russell Erb 03/30/2001
//               Russell Erb 02/27/2002 - check that lastPeriod is > 0
//  			 Chip Gobs 08/2003  -  a number of changes including:
//									   using List instead of Vector
//									   and subclassing from DbTable and DbRecord,
//									   no-conversion-needed handling of the Timestamp data
//                                     The ability to be expanded for use with other
//									   database types.
//				Chip Gobs	9/2004  -  Modified to allow it to work with PostgreSQL or Informix.

package ohd.hseb.dbgen;

//import ohd.hseb.util.*;

import java.util.*;		// needed for List and Map classes and others
import java.util.Date;	// needed for Date class, resolves ambiguity with java.sql.Date
import java.io.*;		// needed for file I/O
import java.sql.*;

import ohd.hseb.db.Database;


public class  JDbGen 
{
//  ---------------------------------------------------------------
// private data
//---------------------------------------------------------------
  	
	private String _generatedPackageName = null;
	private String _targetDirName = null;
	private DataOutputStream _dos = null;
	private List _tableDescriptorList = null;	

	
	private CodeIndenter _indenter = null;
    private final String _indentString = "    ";
    
    
    //for Informix mode
 //   private Map _javaDataTypeStringMap = null; //maps from sqlType String to javaDataType
 //   private Map _rsInterfaceTypeStringMap = null;//maps from sqlTypeString to result set dataType
    

    //for PostgreSQL and other generation modes
    private Map _javaDataTypeIntegerMap = null; //maps from sqlType integer to javaDataType String
    private Map _rsInterfaceTypeIntegerMap = null;   //maps from sqlType integer to result set dataType

    
    // ---------------------------------------------------------------------------------
    
    public JDbGen()
    {
        initJavaDataTypeIntegerMap();
        initResultSetDataTypeIntegerMap();;  
    }
    
    // ---------------------------------------------------------------------------------
 
    private void addJavaDataTypeIntegerMapEntry(int sqlType, String javaType)
    {
        _javaDataTypeIntegerMap.put(new Integer(sqlType), javaType);
    }
    //  -------------------------------------------------------------------------------------
    private void initJavaDataTypeIntegerMap()
    {
        
        _javaDataTypeIntegerMap = new HashMap();
        
        addJavaDataTypeIntegerMapEntry(Types.DATE, "long");
        addJavaDataTypeIntegerMapEntry(Types.TIMESTAMP, "long");
        addJavaDataTypeIntegerMapEntry(Types.TIME, "long");// new one
        
        addJavaDataTypeIntegerMapEntry(Types.REAL, "float");
        addJavaDataTypeIntegerMapEntry(Types.FLOAT, "float"); //changed from double in OB7.2
        addJavaDataTypeIntegerMapEntry(Types.DOUBLE, "double");
        
        addJavaDataTypeIntegerMapEntry(Types.NUMERIC, "double");// new one
         
        addJavaDataTypeIntegerMapEntry(Types.SMALLINT, "short");
        addJavaDataTypeIntegerMapEntry(Types.INTEGER, "int");
        addJavaDataTypeIntegerMapEntry(Types.BIGINT, "long");
        
        addJavaDataTypeIntegerMapEntry(Types.LONGVARBINARY, "byte[]");
        addJavaDataTypeIntegerMapEntry(Types.LONGVARCHAR, "String"); // ? instead of byte[]
        addJavaDataTypeIntegerMapEntry(Types.VARCHAR, "String");
        addJavaDataTypeIntegerMapEntry(Types.CHAR, "String");
        
        addJavaDataTypeIntegerMapEntry(Types.ARRAY, "String");// new one
                   
    }
   
    //-------------------------------------------------------------------------------------
    private void addResultSetTypeIntegerMapEntry(int sqlType, String resultSetType)
    {
        _rsInterfaceTypeIntegerMap.put(new Integer(sqlType), resultSetType);
    }
    //  --------------------------STAMP-----------------------------------------------------------
    private void initResultSetDataTypeIntegerMap()
    {
        _rsInterfaceTypeIntegerMap = new HashMap();
        
        addResultSetTypeIntegerMapEntry(Types.DATE, "Date");
        addResultSetTypeIntegerMapEntry(Types.TIMESTAMP, "TimeStamp");      
        addResultSetTypeIntegerMapEntry(Types.TIME, "Time"); //new one
  
        addResultSetTypeIntegerMapEntry(Types.REAL, "Real");
        addResultSetTypeIntegerMapEntry(Types.FLOAT, "Float");
        addResultSetTypeIntegerMapEntry(Types.DOUBLE, "Double");
        addResultSetTypeIntegerMapEntry(Types.NUMERIC, "Double"); //new one   

        addResultSetTypeIntegerMapEntry(Types.SMALLINT, "Short");
        addResultSetTypeIntegerMapEntry(Types.INTEGER, "Int");
        addResultSetTypeIntegerMapEntry(Types.BIGINT, "Long");
        
        addResultSetTypeIntegerMapEntry(Types.LONGVARBINARY, "Bytes");
        addResultSetTypeIntegerMapEntry(Types.LONGVARCHAR, "String");
        addResultSetTypeIntegerMapEntry(Types.VARCHAR, "String");
        addResultSetTypeIntegerMapEntry(Types.CHAR, "String");
              
        addResultSetTypeIntegerMapEntry(Types.ARRAY, "String"); //new one     
    }
    
    //-------------------------------------------------------------------------------------
    
    //---------------------------------------------------------------
// write and writeLine are used to write out nicely indented code
//---------------------------------------------------------------		
	private void write(String line, boolean useIndentation) throws IOException
	{
	    if (useIndentation)
	    {
	 	   String totalIndentString = _indenter.getTotalIndentString();
	   
		   _dos.writeBytes(totalIndentString + line);	
	    }
	    else
	    {
		   _dos.writeBytes(line);	
	    }
	}
//	 -----------------------------------------------------------------
	private void write(String line) throws IOException
	{
	    write(line, false); //by default, do not use indentation
	  
	    return;
	}
//	 -----------------------------------------------------------------	

	private void writeLine(String line) throws IOException
	{
		boolean useIndentation = true;
	    write(line + "\n", useIndentation);
	  
		return;
	}


// --------------------------------------------------------------------------
	// read the list of preferred table names from a text file with the
	// preferred name of the Tables and Views , 1 per line, no spaces in the name
// --------------------------------------------------------------------------
    private Map getMapOfPreferredTableNames(String filePath)
    {
        Map tableNameMap = new HashMap();
       
        try
        {

		    BufferedReader reader = new BufferedReader (new FileReader(filePath));
		    String line = null;
		     
			line = reader.readLine();
			 
            String preferredTableName = null;
            String lowercaseTableName = null;
            
			while (line != null)
			{
                preferredTableName = null;
                lowercaseTableName = null;
                
			   // System.out.println("line = " + line + ":");
				int separatorIndex = line.indexOf('=');
                
                if (separatorIndex > -1)
                {
                    lowercaseTableName = line.substring(0, separatorIndex).trim().toLowerCase();
                    preferredTableName = line.substring(separatorIndex  + 1).trim();
                }
                else //only one line item
                {
                    preferredTableName = line.trim();
                    lowercaseTableName = preferredTableName.toLowerCase();    
                }
			    
                	
				tableNameMap.put(lowercaseTableName, preferredTableName);		
			 
			    System.out.println("JDbGen.getMapOfPreferredTableNames(): " + 
			            			lowercaseTableName + " -> " + preferredTableName);
				
				line = reader.readLine();
			} //end while
	
			reader.close();
        }
        catch (IOException e)
        {
        	e.printStackTrace();
        }   
       
       
       return tableNameMap;	
    }
	
    
    // ---------------------------------------------------------------------------------
    private Map getMapOfPreferredTableNames_old(String filePath)
    {
        Map tableNameMap = new HashMap();
       
        try
        {

            BufferedReader reader = new BufferedReader (new FileReader(filePath));
            String line = null;
             
            line = reader.readLine();
                   
            while (line != null)
            {
               // System.out.println("line = " + line + ":");
                
                String preferredTableName = line.trim();
                String lowercaseTableName = preferredTableName.toLowerCase();
                
                tableNameMap.put(lowercaseTableName, preferredTableName);       
             
                System.out.println("JDbGen.getMapOfPreferredTableNames(): " + 
                                    lowercaseTableName + " -> " + preferredTableName);
                
                line = reader.readLine();
            } //end while
    
            reader.close();
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }   
       
       
       return tableNameMap; 
    }
    
    
    // ---------------------------------------------------------------------------------
  
    private String getJavaDataTypeByInt(int sqlType)
    {
        String javaDataType = null;
        
        javaDataType = (String) _javaDataTypeIntegerMap.get(new Integer(sqlType));
        if (javaDataType == null)
        {
            javaDataType = "UNKNOWN_SQL_TYPE_" + sqlType;
        }
        
        return javaDataType;
    }
    // ---------------------------------------------------------------------------------
     
    private String getRSInterfaceTypeByInt(int sqlType)
    {
        String resultSetDataType = null;
        
        resultSetDataType = (String) _rsInterfaceTypeIntegerMap.get(new Integer(sqlType));
        if (resultSetDataType == null)
        {
            resultSetDataType = "UNKNOWN_SQL_TYPE_" + sqlType;
        }
        
        return resultSetDataType;     
        
    }
   
    // ---------------------------------------------------------------------------------
    // buildRecordSrcFiles

    private void buildRecordSrcFiles(String dbName)
    {
        try
        {
            // Strings to hold Table name and File name
            String tableName = null;
            String fileName  = null;

            // loop through all the table names and build source files
            for (int ctr=0; ctr < _tableDescriptorList.size(); ctr++)
            {
                TableDescriptor tableDesc = (TableDescriptor) _tableDescriptorList.get(ctr);
                tableName = tableDesc.getName();

                fileName = tableName + "Record.java";

                FileOutputStream fos = new FileOutputStream(_targetDirName + "/" + fileName);
                _dos = new DataOutputStream(fos);

                createRecordFileBanner(tableName, fileName, dbName, tableDesc);
                createRecordPrivateData(tableDesc);
                createRecordEmptyConstructor(tableName);
                createRecordCopyConstructor(tableName, tableDesc);
                createRecordSetGet(tableName, tableDesc);

                if ( (! tableDesc.isView()) && (tableDesc.hasPrimaryKey()) )
                {
                    createRecordGetWhereString(tableDesc);
                }
                createRecordToString(tableDesc);

                _indenter.setLevel(0);
                writeLine("} // end of " + tableName +"Record class\n");

                _dos.close();
                //fos.close();
            }
        }
        catch(IOException ioe)
        {
            System.err.println( "buildRecordSrcFiles(): Something went wrong trying to write " + ioe);
        }

    } //  end of buildRecordSrcFiles method


//  -----------------------------------------------------------------
//  createRecordFileBanner
//  -----------------------------------------------------------------
    private void createRecordFileBanner(String tableName,
            String fileName,
            String dbName,
            TableDescriptor tableDesc)
    {
        try
        {
            _indenter.setLevel(0);
            // current time
            Date currentTime = new Date();

            System.out.println("building source code for " + fileName);

            if (tableDesc.isView())
            {
                writeLine("// This is a view record !");	
            }

            writeLine("// filename: " + fileName);
            writeLine("// author  : DBGEN");
            writeLine("// created : " + currentTime + " using database " + dbName);
            writeLine("// description: This class is used to get data from and put data into a");
            writeLine("//              " + tableName + " table record format");
            writeLine("//\n");

            if (_generatedPackageName.length() > 0)
            {
                writeLine("package " + _generatedPackageName + ";\n");
            }

            writeLine("import ohd.hseb.db.*;\n");
            writeLine("public class " + tableName +"Record extends DbRecord");
            writeLine("{");
        }
        catch(IOException ioe)
        {
            System.out.println( "createRecordFileBanner(): Something went wrong trying to write " + ioe);
        }

    } //  end of createRecordFileBanner method



//  -----------------------------------------------------------------
//  createRecordPrivateData
//  -----------------------------------------------------------------
    private void createRecordPrivateData(TableDescriptor tableDesc)
    {
        try
        {
            List columnDescriptorList = tableDesc.getColumnDescriptorList();

            _indenter.setLevel(1);

            for (int i = 0 ; i < columnDescriptorList.size(); i++)
            {
                ColumnDescriptor columnDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                String javaDataType = getJavaDataTypeByInt(columnDesc.getSqlTypeInt());
                String columnName = columnDesc.getName();

                writeLine("private " + javaDataType + " " + columnName + ";\n");
            }

        }
        catch(IOException ioe)
        {
            System.err.println( "createRecordPrivateData(): Something went wrong trying to write " + ioe);
        }

    } //  end of createRecordPrivateData method

//  ---------------------------------------------------------
//  createRecordEmptyConstructor
//  --------------------------------------------------------
    private void createRecordEmptyConstructor(String tableName)
    {

        try
        {
            writeLine("//---------------------------------------------------------------");
            writeLine("// Empty constructor");
            writeLine("//---------------------------------------------------------------");

            _indenter.setLevel(1);
            writeLine("public " + tableName +"Record()");
            writeLine("{");
            writeLine("}\n");

        }
        catch(IOException ioe)
        {
            System.err.println( "createRecordEmptyConstructor(): Something went wrong trying to write " + ioe);
        }

    } //  end of createRecordEmptyConstructor method


//  ---------------------------------------------------------
//  createRecordCopyConstructor
//  --------------------------------------------------------
    private void createRecordCopyConstructor(String tableName,
            TableDescriptor tableDesc)
    {

        List columnDescriptorList = tableDesc.getColumnDescriptorList();

        try
        {
            writeLine("//-----------------------------------------------------------------");
            writeLine("// Copy constructor");
            writeLine("//-----------------------------------------------------------------");

            _indenter.setLevel(1);
            writeLine("public " + tableName +"Record(" + tableName + "Record origRecord)");
            writeLine("{");
            _indenter.incLevel();

            for (int i = 0; i < columnDescriptorList.size(); i++)
            {
                ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);

                String columnName = colDesc.getName();

                String firstChar = String.valueOf(columnName.charAt(0));
                String newName = firstChar.toUpperCase() + columnName.substring(1);

                writeLine("set" + newName + "(origRecord.get" + newName + "());");  

            } //end for each column

            _indenter.decLevel();
            writeLine("}\n");

        }
        catch(IOException ioe)
        {
            System.err.println( "createRecordCopyConstructor(): Something went wrong trying to write " + ioe);
        }

    } //  end of createRecordCopyConstructor method

//  ---------------------------------------------------------------
//  createRecordSetGet
//  ---------------------------------------------------------------
    private void createRecordSetGet(String tableName,
            TableDescriptor tableDesc)
    {

        List columnDescriptorList = tableDesc.getColumnDescriptorList();

        try
        {
            writeLine("//-----------------------------------------------------------------");
            writeLine("//  get and set methods for all data items in a " + tableName + " record\n");
            writeLine("//-----------------------------------------------------------------");


            for (int i = 0; i < columnDescriptorList.size(); i++)
            {
                ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                String javaDataType = getJavaDataTypeByInt(colDesc.getSqlTypeInt());
                String columnName = colDesc.getName();

                String firstChar = String.valueOf(columnName.charAt(0));
                String newName = firstChar.toUpperCase() + columnName.substring(1);


                //create the getter
                writeLine("public " + javaDataType + " get" + newName + "()");

                writeLine("{");

                _indenter.incLevel();
                writeLine("return " + columnName + ";" );

                _indenter.decLevel();
                writeLine("}\n");

                //create the setter
                writeLine("public void set" + newName + "(" + javaDataType + " " + columnName + ")");
                writeLine("{");

                _indenter.incLevel();
                writeLine("this." + columnName + " = " + columnName + " ;");

                _indenter.decLevel();
                writeLine("}\n");

            }

        }
        catch(IOException ioe)
        {
            System.err.println( "createRecordSetGet(): Something went wrong trying to write " + ioe);
        }

    } //  end of createRecordSetGet method

//  ---------------------------------------------------------------
//  createRecordGetWhereString
//  ---------------------------------------------------------------
    private void createRecordGetWhereString(TableDescriptor tableDesc)
    {
        try
        {
            _indenter.setLevel(0);
            writeLine("//-----------------------------------------------------------------");
            writeLine("//  getWhereString() - this method is called with no arguments");
            writeLine("//  and returns a String that contains a valid where clause containing all the");
            writeLine("//  primary key fields.");
            writeLine("//-----------------------------------------------------------------");

            _indenter.incLevel();
            writeLine("public String getWhereString()");
            writeLine("{");

            _indenter.incLevel();
            writeLine("String outString = ");

            _indenter.incLevel();
            _indenter.incLevel(); 

            List columnDescriptorList = tableDesc.getColumnDescriptorList();

            write("\"WHERE ", true);

            int keyColumnCount = 0;

            for (int i = 0 ; i < columnDescriptorList.size(); i++)
            {
                ColumnDescriptor columnDesc = (ColumnDescriptor) columnDescriptorList.get(i);

                // String sqlDataType = columnDesc.getSqlTypeString();
                int sqlDataTypeInt = columnDesc.getSqlTypeInt();

                //String javaDataType = columnDesc.getJavaType();
                String columnName = columnDesc.getName();

                boolean isKeyColumn = columnDesc.isKeyColumn();

                //append the pieces to the WHERE clause
                if (isKeyColumn)
                {
                    keyColumnCount++;
                    if (keyColumnCount != 1) //not first one
                    {
                        write(" + \" AND ", true);	
                    }


                    /*
			 	 if (sqlDataType.equalsIgnoreCase("DateTime"))
			 	 {
				     write(columnName + " = '\" +  getDateTimeStringFromLongTime(" + columnName + ")" + " + \"'\" \n");
			 	 }
			 	 else if (sqlDataType.equalsIgnoreCase("Date"))
			 	 {
					 write(columnName + " = '\" +  getDateStringFromLongTime(" + columnName + ")" + " + \"'\" \n");
			 	 }
                     */
                    if (sqlDataTypeInt == Types.TIMESTAMP)
                    {
                        write(columnName + " = '\" +  getDateTimeStringFromLongTime(" + columnName + ")" + " + \"'\" \n");
                    }
                    else if (sqlDataTypeInt == Types.DATE)
                    {
                        write(columnName + " = '\" +  getDateStringFromLongTime(" + columnName + ")" + " + \"'\" \n");
                    }

                    else //the usual
                    {	
                        write(columnName + " = '\" + " + columnName + " + \"'\" \n");
                    }


                } //end isKeyColumn

            }//end for loop

            writeLine(";");  


            //writeLine("\"\" ;");

            //get rid of extra indents
            _indenter.decLevel();
            _indenter.decLevel();

            writeLine("return outString;");

            _indenter.decLevel();
            writeLine("} // end toString()");

        }	
        catch(IOException ioe)
        {
            System.err.println( "createRecordGetWhereString(): Something went wrong trying to write " + ioe);
        }

    } //  end of CreateRecordGetWhereString method



//  ---------------------------------------------------------------
//  createRecordToString
//  ---------------------------------------------------------------
    private void createRecordToString(TableDescriptor tableDesc)
    {
        try
        {
            _indenter.setLevel(0);
            writeLine("//-----------------------------------------------------------------");
            writeLine("//  toString() - this method is called with no arguments");
            writeLine("//  and returns a String of the internal values");
            writeLine("//-----------------------------------------------------------------");

            _indenter.incLevel();
            writeLine("public String toString()");
            writeLine("{");

            _indenter.incLevel();
            writeLine("String outString = ");

            _indenter.incLevel();
            _indenter.incLevel(); 

            List columnDescriptorList = tableDesc.getColumnDescriptorList();

            for (int i = 0 ; i < columnDescriptorList.size(); i++)
            {
                ColumnDescriptor columnDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                // String sqlDataType = columnDesc.getSqlTypeString();
                int sqlDataTypeInt = columnDesc.getSqlTypeInt();
                //String javaDataType = columnDesc.getJavaType();
                String columnName = columnDesc.getName();

                String firstChar = String.valueOf(columnName.charAt(0));
                String newName = firstChar.toUpperCase() + columnName.substring(1);

                /*
			 if (sqlDataType.equalsIgnoreCase("DateTime"))
			 {
				writeLine("getDateTimeStringFromLongTime(get" + newName + "()) + \" \" +");
			 }
			 else if (sqlDataType.equalsIgnoreCase("Date"))
			 {
				writeLine("getDateStringFromLongTime(get" + newName + "()) + \" \" +");
			 }
                 */

                if (sqlDataTypeInt == Types.TIMESTAMP)
                {
                    writeLine("getDateTimeStringFromLongTime(get" + newName + "()) + \" \" +");
                }
                else if (sqlDataTypeInt == Types.DATE)
                {
                    writeLine("getDateStringFromLongTime(get" + newName + "()) + \" \" +");
                }
                else
                {
                    writeLine("get" + newName + "() + \" \" +");
                }
            }	   

            writeLine("\"\" ;");

            //get rid of extra indents
            _indenter.decLevel();
            _indenter.decLevel();

            writeLine("return outString;");

            _indenter.decLevel();
            writeLine("} // end toString()");

        }	
        catch(IOException ioe)
        {
            System.err.println( "createRecordToString(): Something went wrong trying to write " + ioe);
        }

    } //  end of CreateRecordToString method


//  ---------------------------------------------------------------
//  buildTableSrcFiles
//  ---------------------------------------------------------------
    private void buildTableSrcFiles(String dbName)
    {

        try
        {
            // Strings to hold Table name and File name
            String tableName = null;
            String originalTableName = null;
            String fileName  = null;

            // loop through all the table names and build source files
            for (int ctr=0; ctr < _tableDescriptorList.size(); ctr++)
            {
                TableDescriptor tableDesc = (TableDescriptor) _tableDescriptorList.get(ctr);
                tableName = tableDesc.getName();
                originalTableName = tableDesc.getOriginalCaseTableName();

                if (tableDesc.isView())
                {
                    fileName = tableName + "View.java";
                }
                else
                {
                    fileName = tableName + "Table.java";
                }

                FileOutputStream fos = new FileOutputStream(_targetDirName + "/" + fileName);
                _dos = new DataOutputStream(fos);

                createTableFileBanner(tableName, fileName, dbName, tableDesc);
                createTableData();

                createTableConstructor(tableName, tableDesc);
                createTableSelect(tableName, originalTableName, tableDesc);
                createTableSelectNRecords(tableName, originalTableName, tableDesc);

                if (! tableDesc.isView()) //is a regular table
                {    
                    createTableInsert(tableName, tableDesc);

                    //all regular tables have this
                    createTableDelete1(tableName, originalTableName);
                    createTableUpdate1(tableName, originalTableName, tableDesc);

                    // only regular tables with primary keys also have this
                    if (tableDesc.hasPrimaryKey())
                    {
                        createTableDelete2(tableName, originalTableName);
                        createTableUpdate2(tableName, originalTableName, tableDesc);
                        createTableInsertOrUpdate(tableName);
                    }

                }

                _indenter.setLevel(0);
                writeLine("} // end of " + tableName +"Table class");

                _dos.close();
                //fos.close();
            }
        }
        catch(IOException ioe)
        {
            System.out.println( "buildTableSrcFiles(): Something went wrong trying to write " + ioe);
        }

    } //  end of buildTableSrcFiles method


//  ---------------------------------------------------------------
//  createTableFileBanner
//  ---------------------------------------------------------------
    private void createTableFileBanner(String tableName,
            String fileName,
            String dbName,
            TableDescriptor tableDesc)
    {
        try
        {
            // current time
            Date currentTime = new Date();

            System.out.println("building source code for " + fileName);

            _indenter.setLevel(0);

            writeLine("// filename: " + fileName);
            writeLine("// author  : DBGEN");
            writeLine("// created : " + currentTime + " using database " + dbName);
            writeLine("// description: This class is used to get data from and put data into the");
            writeLine("//              " + tableDesc.getOriginalCaseTableName() + " table of an IHFS database");
            writeLine("//");

            if (_generatedPackageName.length() > 0)
            {
                writeLine("package " + _generatedPackageName + ";\n");
            }

            writeLine("import java.sql.*;\n");
            writeLine("import java.util.*;\n");
            writeLine("import ohd.hseb.db.*;\n");

            if (tableDesc.isView())
            {
                writeLine("public class " + tableName +"View extends DbTable");
            }
            else //regular table
            {
                writeLine("public class " + tableName +"Table extends DbTable");
            }

            writeLine("{");
        }
        catch(IOException ioe)
        {
            System.out.println( "createTableFileBanner(): Something went wrong trying to write " + ioe);
        }

    } //  end of createTableFileBanner method


//  ---------------------------------------------------------------
//  createTableConstructors
//  ---------------------------------------------------------------
    private void createTableData()
    {
        try
        {

            writeLine("//-----------------------------------------------------------------");
            writeLine("//  Private data");
            writeLine("//-----------------------------------------------------------------");

            _indenter.setLevel(1);
            writeLine("private int _recordsFound = -1;");


        }
        catch(IOException ioe)
        {
            System.err.println( "createTableData(): Something went wrong trying to write " + ioe);
        }

    } //  end of createTableData method

//---------------------------------------------------------------
//createTableConstructor
//---------------------------------------------------------------
    private void createTableConstructor(String tableName, TableDescriptor tableDesc)
    {
        
        String originalTableName = tableDesc.getOriginalCaseTableName();
        
        try
  	    {
  	    	_indenter.setLevel(0);
			writeLine("//-----------------------------------------------------------------");
			writeLine("//  " + tableName + "Table() - constructor to set statement variable and initialize");
			writeLine("//\t\tnumber of records found to zero");
			writeLine("//-----------------------------------------------------------------");
		
			_indenter.incLevel();
			
			if (tableDesc.isView())
			{
				writeLine("public " + tableName + "View(Database database) ");
			}
			else
			{
			    writeLine("public " + tableName + "Table(Database database) ");
			}
			
			writeLine("{");
			
			_indenter.incLevel();
  	        writeLine("//Constructor calls DbTable's constructor");
  	        writeLine("super(database);");
			writeLine("setTableName(\"" + tableDesc.getOriginalCaseTableName() + "\");");
			
			_indenter.decLevel();
  	        writeLine("}\n\n");	
		
  	    }
  	    catch (IOException e)
  	    {
  	    	e.printStackTrace();
  	    }
  } //end createTableConstructor
//---------------------------------------------------------------
// createTableSelect
//---------------------------------------------------------------
    private void createTableSelect(String tableName, String originalTableName, TableDescriptor tableDesc)
    {

        try
        {
            writeLine("//-----------------------------------------------------------------");
            writeLine("//  select() - this method is called with a where clause and returns");
            writeLine("//\t\ta List of " + tableName + "Record objects");
            writeLine("//-----------------------------------------------------------------");

            _indenter.setLevel(1);
            writeLine("public List select(String where) throws SQLException");
            writeLine("{");

            _indenter.incLevel();
            writeLine(tableName + "Record record = null;\n");
            writeLine("// create a List to hold " + tableName + " Records");
            writeLine("List recordList = new ArrayList();\n");
            writeLine("// set number of records found to zero");
            writeLine("_recordsFound = 0;\n");
            writeLine("// Create the SQL statement and issue it");

            writeLine("// construct the select statment");
            writeLine("String selectStatement = \"SELECT * FROM " +  originalTableName + " \" + where;\n");
            writeLine("// get the result set back from the query to the database");
            writeLine("ResultSet rs = getStatement().executeQuery(selectStatement);\n");
            writeLine("// loop through the result set");
            writeLine("while (rs.next())");
            writeLine("{");

            _indenter.incLevel();
            writeLine("// create an instance of a " + tableName +"Record");
            writeLine("// and store its address in oneRecord");
            writeLine("record = new " + tableName +"Record();\n");
            writeLine("// increment the number of records found");
            writeLine("_recordsFound++;\n");
            writeLine("// assign the data returned to the result set for one");
            writeLine("// record in the database to a " + tableName +"Record object\n");

            List columnDescriptorList = tableDesc.getColumnDescriptorList();

            int index = -1;

            for (int i = 0 ; i < columnDescriptorList.size(); i++)
            {
                index = i + 1;
                ColumnDescriptor columnDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                //String sqlDataType = columnDesc.getSqlTypeString();

                int sqlDataTypeInt = columnDesc.getSqlTypeInt();


                String rsInterfaceType = getRSInterfaceTypeByInt(sqlDataTypeInt);

                String columnName = columnDesc.getName();
                String firstChar = String.valueOf(columnName.charAt(0));
                String newName = firstChar.toUpperCase() + columnName.substring(1);

                writeLine("record.set" + newName + "(get" + rsInterfaceType + "(rs, " + index + "));");

            } //end for

            writeLine("");

            writeLine("// add this " + tableName +"Record object to the list");
            writeLine("recordList.add(record);");

            _indenter.decLevel();
            writeLine("}");
            writeLine("// Close the result set");
            writeLine("rs.close();\n");

            //create the code to return the recordList
            writeLine("// return a List which holds the " + tableName +"Record objects");

            writeLine("return recordList;\n");

            _indenter.decLevel();
            writeLine("} // end of select method\n");
        }
        catch(IOException ioe)
        {
            System.err.println( "createTableSelect(): Something went wrong trying to write " + ioe);
        }

    } //  end of createTableSelect method

//  ---------------------------------------------------------------
//  createTableSelect
//  ---------------------------------------------------------------
    private void createTableSelectNRecords(String tableName, String originalTableName, TableDescriptor tableDesc)
    {
        try
        {
            writeLine("//-----------------------------------------------------------------");
            writeLine("//  selectNRecords() - this method is called with a where clause and returns");
            writeLine("//\t\ta List filled with a maximum of maxRecordCount of " + tableName + "Record objects ");
            writeLine("//-----------------------------------------------------------------");

            _indenter.setLevel(1);
            writeLine("public List selectNRecords(String where, int maxRecordCount) throws SQLException");
            writeLine("{");

            _indenter.incLevel();
            writeLine(tableName + "Record record = null;\n");
            writeLine("// create a List to hold " + tableName + " Records");
            writeLine("List recordList = new ArrayList();\n");
            writeLine("// set number of records found to zero");
            writeLine("_recordsFound = 0;\n");
            writeLine("// Create the SQL statement and issue it");

            writeLine("// construct the select statment");
            writeLine("String selectStatement = \"SELECT * FROM " + originalTableName + " \" + where;\n");
            writeLine("// get the result set back from the query to the database");
            writeLine("ResultSet rs = getStatement().executeQuery(selectStatement);\n");
            writeLine("// loop through the result set");
            writeLine("while (rs.next())");
            writeLine("{");

            _indenter.incLevel();
            writeLine("// create an instance of a " + tableName +"Record");
            writeLine("// and store its address in oneRecord");
            writeLine("record = new " + tableName +"Record();\n");
            writeLine("// increment the number of records found");
            writeLine("_recordsFound++;\n");
            writeLine("// assign the data returned to the result set for one");
            writeLine("// record in the database to a " + tableName +"Record object\n");

            List columnDescriptorList = tableDesc.getColumnDescriptorList();

            int index = -1;

            for (int i = 0 ; i < columnDescriptorList.size(); i++)
            {
                index = i + 1;
                ColumnDescriptor columnDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                //String sqlDataType = columnDesc.getSqlTypeString();
                int sqlDataTypeInt = columnDesc.getSqlTypeInt();
                String rsInterfaceType = getRSInterfaceTypeByInt(sqlDataTypeInt);
                //String javaDataType = columnDesc.getJavaType();
                String columnName = columnDesc.getName();
                String firstChar = String.valueOf(columnName.charAt(0));
                String newName = firstChar.toUpperCase() + columnName.substring(1);

                writeLine("record.set" + newName + "(get" + rsInterfaceType + "(rs, " + index + "));");

            } //end for

            writeLine("");

            writeLine("// add this " + tableName +"Record object to the list");
            writeLine("recordList.add(record);");

            writeLine("if (_recordsFound >= maxRecordCount)");
            writeLine("{");
            _indenter.incLevel();

            writeLine("break;");

            _indenter.decLevel();
            writeLine("}");

            _indenter.decLevel();
            writeLine("}");

            writeLine("// Close the result set");
            writeLine("rs.close();\n");

            //create the code to return the recordList
            writeLine("// return a List which holds the " + tableName +"Record objects");

            writeLine("return recordList;\n");

            _indenter.decLevel();
            writeLine("} // end of selectNRecords method\n");
        }
        catch(IOException ioe)
        {
            System.err.println( "createTableSelectNRecords(): Something went wrong trying to write " + ioe);
        }

    } //  end of createTableSelectNRecords method
//  ---------------------------------------------------------------
//  createTableInsert
//  ---------------------------------------------------------------
    private void createTableInsert(String tableName, TableDescriptor tableDescriptor)
    {
        String originalTableName = tableDescriptor.getOriginalCaseTableName();

        try
        {
            _indenter.setLevel(0);
            writeLine("//-----------------------------------------------------------------");
            writeLine("//  insert() - this method is called with a " + tableName + "Record object and..");
            writeLine("//-----------------------------------------------------------------");

            _indenter.incLevel();
            writeLine("public int insert(" + tableName + "Record record)  throws SQLException");
            writeLine("{");

            _indenter.incLevel();
            writeLine("int returnCode=-999;\n");
            writeLine("// Create a SQL insert statement and issue it");


            writeLine("// construct the insert statement");
            writeLine("PreparedStatement insertStatement = getConnection().prepareStatement(");
            write("\" INSERT INTO " +  originalTableName + " VALUES (");

            List columnDescriptorList = tableDescriptor.getColumnDescriptorList();


            for (int i = 0 ; i < columnDescriptorList.size(); i++)
            {
                if (i == 0)
                {
                    write("?");
                }
                else
                {
                    write(", ?");
                } 
            }

            writeLine(")\");\n");

            int index = -1;
            for (int i = 0 ; i < columnDescriptorList.size(); i++)
            {
                index = i+1;
                ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);

                int sqlDataTypeInt = colDesc.getSqlTypeInt();
                String rsInterfaceType = getRSInterfaceTypeByInt(sqlDataTypeInt);
                //String sqlDataType = colDesc.getSqlTypeString();

                String columnName = colDesc.getName();

                String firstChar = String.valueOf(columnName.charAt(0));
                String newName = firstChar.toUpperCase() + columnName.substring(1);

                writeLine("set" + rsInterfaceType + "(insertStatement, " + index + ", record.get" + newName + "());");

            }

            writeLine("");

            writeLine("// get the number of records processed by the insert");
            writeLine("returnCode = insertStatement.executeUpdate();\n");
            //writeLine("System.out.println(\"returnCode from putRecord = \" + returnCode);");

            writeLine("return returnCode;\n");

            _indenter.decLevel();
            writeLine("} // end of insert method\n");
        }
        catch(IOException ioe)
        {
            System.err.println( "createTableInsert(): Something went wrong trying to write " + ioe);
        }

    } //  end of createTableInsert method


//  ---------------------------------------------------------------
//  createTableDelete1
//  ---------------------------------------------------------------
    private void createTableDelete1(String tableName, String originalTableName)
    {

        try
        {
            _indenter.setLevel(0);
            writeLine("//-----------------------------------------------------------------");
            writeLine("//  delete() - this method is called with a where clause and returns");
            writeLine("//                   the number of records deleted");
            writeLine("//-----------------------------------------------------------------");

            _indenter.incLevel();
            writeLine("public int delete(String where) throws SQLException");
            writeLine("{");

            _indenter.incLevel();
            writeLine("int returnCode=-999;\n");
            writeLine("// Create a SQL delete statement and issue it");

            writeLine("// construct the delete statement");
            writeLine("String deleteStatement = \"DELETE FROM " +  originalTableName + " \" + where;\n");
            writeLine("// get the number of records processed by the delete");
            writeLine("returnCode = getStatement().executeUpdate(deleteStatement);\n");

            //writeLine("System.out.println(\"returnCode from delete = \" + returnCode);");

            writeLine("return returnCode;");

            _indenter.decLevel();        
            writeLine("} // end of delete method \n");
        }
        catch(IOException ioe)
        {
            System.err.println( "createTableDelete1(): Something went wrong trying to write " + ioe);
        }

    } //  end of createDelete1 method

//  ---------------------------------------------------------------
//  createTableDelete2
//  ---------------------------------------------------------------
    private void createTableDelete2(String tableName, String originalTableName)
    {

        try
        {
            _indenter.setLevel(0);
            writeLine("//-----------------------------------------------------------------");
            writeLine("//  delete() - this method is called with a where clause and returns");
            writeLine("//                   the number of records deleted");
            writeLine("//-----------------------------------------------------------------");

            _indenter.incLevel();
            writeLine("public int delete(" + tableName + "Record record) throws SQLException");
            writeLine("{");

            _indenter.incLevel();
            writeLine("int returnCode=-999;\n");
            writeLine("// Create a SQL delete statement and issue it");

            writeLine("// construct the delete statement");
            writeLine("String deleteStatement = \"DELETE FROM " +  originalTableName + " \" + record.getWhereString();\n");
            writeLine("// get the number of records processed by the delete");
            writeLine("returnCode = getStatement().executeUpdate(deleteStatement);\n");

            //writeLine("System.out.println(\"returnCode from delete = \" + returnCode);");

            writeLine("return returnCode;");

            _indenter.decLevel();        
            writeLine("} // end of delete method \n");
        }
        catch(IOException ioe)
        {
            System.err.println( "createTableDelete2(): Something went wrong trying to write " + ioe);
        }

    } //  end of createDelete2 method


//  ---------------------------------------------------------------
//  createTableUpdate1
//  ---------------------------------------------------------------
    private void createTableUpdate1(String tableName, String originalTableName, TableDescriptor tableDesc)
    {

        try
        {
            _indenter.setLevel(0);


            writeLine("//-----------------------------------------------------------------");
            writeLine("//  update() - this method is called with a " + tableName + "Record object and a where clause..");
            writeLine("//-----------------------------------------------------------------");

            _indenter.incLevel();
            writeLine("public int update(" + tableName + 
            "Record record, String where)  throws SQLException");
            writeLine("{");

            _indenter.incLevel();
            writeLine("int returnCode=-999;");
            writeLine("// Create a SQL update statement and issue it");

            writeLine("// construct the update statement");
            writeLine("PreparedStatement updateStatement = getConnection().prepareStatement(");
            write("\" UPDATE " +  originalTableName + " SET ");

            List columnDescriptorList = tableDesc.getColumnDescriptorList();


            for (int i = 0 ; i < columnDescriptorList.size(); i++)
            {
                ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                String columnName = colDesc.getName();

                if (i == 0)
                {
                    write(columnName + " = ?");

                }
                else
                {
                    write(", " + columnName + " = ?");
                }

            } //end for	

            writeLine("\" + where );\n");
            int index = -1;

            for (int i = 0 ; i < columnDescriptorList.size(); i++)
            {
                index = i+1;
                ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                String columnName = colDesc.getName();

                //String sqlDataType = colDesc.getSqlTypeString();
                int sqlDataTypeInt = colDesc.getSqlTypeInt();
                String rsInterfaceType = getRSInterfaceTypeByInt(sqlDataTypeInt);
                String firstChar = String.valueOf(columnName.charAt(0));

                String newName = firstChar.toUpperCase() + columnName.substring(1);

                writeLine("set" + rsInterfaceType + "(updateStatement, " + index + ", record.get" + newName + "());");              

            } //end for


            writeLine("// get the number of records processed by the update");
            writeLine("returnCode = updateStatement.executeUpdate();\n");
            //writeLine("System.out.println(\"returnCode from updateRecord = \" + returnCode);");

            writeLine("return returnCode;\n");

            _indenter.decLevel();
            writeLine("} // end of updateRecord method\n");
        }

        catch(IOException ioe)
        {
            System.err.println( "createTableUpdate1(): Something went wrong trying to write " + ioe);
        }

    } //  end of createTableUpdate1 method

//  ---------------------------------------------------------------
//  createTableUpdate2
//  ---------------------------------------------------------------
    private void createTableUpdate2(String tableName, String originalTableName, TableDescriptor tableDesc)
    {

        try
        {
            _indenter.setLevel(0);


            writeLine("//-----------------------------------------------------------------");
            writeLine("//  update() - this method is called with a " + tableName + "Record object and a where clause..");
            writeLine("//-----------------------------------------------------------------");

            _indenter.incLevel();
            writeLine("public int update(" + tableName + "Record oldRecord, " + 
                    tableName + "Record newRecord)  throws SQLException");
            writeLine("{");

            _indenter.incLevel();
            writeLine("int returnCode=-999;");
            writeLine("// Create a SQL update statement and issue it");

            writeLine("// construct the update statement");
            writeLine("PreparedStatement updateStatement = getConnection().prepareStatement(");
            write("\" UPDATE " +  originalTableName + " SET ");

            List columnDescriptorList = tableDesc.getColumnDescriptorList();

            for (int i = 0 ; i < columnDescriptorList.size(); i++)
            {
                ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                String columnName = colDesc.getName();

                if (i == 0)
                {
                    write(columnName + " = ?");

                }
                else
                {
                    write(", " + columnName + " = ?");
                }

            } //end for	

            writeLine("\" + oldRecord.getWhereString() );\n");
            int index = -1;

            for (int i = 0 ; i < columnDescriptorList.size(); i++)
            {
                index = i+1;
                ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                String columnName = colDesc.getName();
                //String sqlDataType = colDesc.getSqlTypeString();
                int sqlDataTypeInt = colDesc.getSqlTypeInt();
                String rsInterfaceType = getRSInterfaceTypeByInt(sqlDataTypeInt);
                String firstChar = String.valueOf(columnName.charAt(0));

                String newName = firstChar.toUpperCase() + columnName.substring(1);

                writeLine("set" + rsInterfaceType + "(updateStatement, " + index + ", newRecord.get" + newName + "());");

            } //end for


            writeLine("// get the number of records processed by the update");
            writeLine("returnCode = updateStatement.executeUpdate();\n");
            //writeLine("System.out.println(\"returnCode from updateRecord = \" + returnCode);");

            writeLine("return returnCode;\n");

            _indenter.decLevel();
            writeLine("} // end of updateRecord method\n");
        }

        catch(IOException ioe)
        {
            System.err.println( "createTableUpdate2(): Something went wrong trying to write " + ioe);
        }

    } //  end of createTableUpdate2 method

    private void createTableInsertOrUpdate(String tableName)
    {

        try
        { 
            _indenter.setLevel(0);
            writeLine("//-----------------------------------------------------------------");
            writeLine("//  insertOrUpdate() - this method is call with a " + tableName + "Record object.");
            writeLine("//                   the number of records inserted or updated");
            writeLine("//-----------------------------------------------------------------");

            _indenter.incLevel();
            writeLine("public int insertOrUpdate(" + tableName + "Record record) throws SQLException");
            writeLine("{");
            _indenter.incLevel();
            writeLine("int returnCode=-999;");
            writeLine("List recordList = select(record.getWhereString());\n");

            writeLine("if (recordList.size() < 1)");
            writeLine("{");
            _indenter.incLevel();
            writeLine("returnCode = insert(record);");
            _indenter.decLevel();
            writeLine("}");

            writeLine("else");
            writeLine("{");
            _indenter.incLevel();

            writeLine(tableName + "Record oldRecord = (" + tableName + "Record) recordList.get(0);");
            writeLine("returnCode = update(oldRecord, record);");
            _indenter.decLevel();
            writeLine("}");

            writeLine("return returnCode;");

            _indenter.decLevel();
            writeLine("} // end of insertOrUpdate() ");
        }
        catch(IOException ioe)
        {
            System.err.println( "createTableInsertOrUpdate(): Something went wrong trying to write " + ioe);
        }
    } //end of createTableInsertOrUpdate()



//  ---------------------------------------------------------------
//  generate - calls all the methods to cause code to be generated
//  ---------------------------------------------------------------


    public void generate(String connectionURL, String preferredTableNameFilePath,
            String dbName, String generatedPackageName,
            String targetDir, String driverName)
    {


//      define an instance of a the IHFS database class
        Database currentDatabase = new Database();

        if (driverName != null)
        {
            currentDatabase.setDriverClassName(driverName);
        }

        // call the method to connect to the database
        currentDatabase.connect(connectionURL);

        // store the name of the package to which the generated classes will belong
        _generatedPackageName = generatedPackageName;

        //store the path of the target directory for the generated files
        _targetDirName = targetDir;

        //reads a list of Preferred table names from the directory indicated by
        //schemaDirName
        Map preferredTableNameMap = 
            getMapOfPreferredTableNames(preferredTableNameFilePath);

        // returns a list of the TableDescriptor which contains metadata about the
        // tables and columns.  This is the method that needs to be overridden when
        // a database driver requires a custom way of extracting the metadata.
        SchemaDescriber finder = new SchemaDescriber(currentDatabase);


        _tableDescriptorList =
            finder.getTableDescriptorList(preferredTableNameMap);


        _indenter  = new CodeIndenter(_indentString);

        //generate all the XXXRecord.java files
        buildRecordSrcFiles(dbName);

        //generate all the XXXTable.jar files
        buildTableSrcFiles(dbName);

        // disconnect from the database
        currentDatabase.disconnect();

    } //end generate

//  ---------------------------------------------------------------

//  ---------------------------------------------------------------
    public static void main(String args[])
    {

        //	CodeTimer timer = new CodeTimer();
        //	timer.start();

        String connectionURL = null;
        String preferredTableNameFilePath = null;
        // the database name is passed in as the third command line argument
        String dbName = null;
        String packageName = null;
        String targetDir = null;
        String driverName = null;


        if (args.length < 5)
        {
            System.err.println("Usage:  java JDbGen connectionURL preferredTableNameFilePath dbName packageName targetDir [driverClassName]"); 
        }
        else // the argument count looks good
        {
            connectionURL = args[0];
            preferredTableNameFilePath = args[1];

            // the database name is passed in as the third command line argument
            dbName = args[2];

            packageName = args[3];
            if (packageName.equals("NONE"))
            {
                packageName = "";   
            }

            targetDir = args[4];

            if (args.length > 5)
            {
                driverName = args[5];

            }

            // create a JDbGen object named dbgen
            JDbGen dbgen = new  JDbGen();

            dbgen.generate(connectionURL, preferredTableNameFilePath, dbName, packageName, targetDir, driverName);

            System.out.println( "JDBGEN completed!");
            //timer.stop("JDbGen took");
        }

        return;

    } // end of main

} // end of  JDbGen class
