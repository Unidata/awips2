//
// //  File: CDbGen.java
//Orig Author: Chip Gobs 10/6/04
//Spawned from JDbGen

//This is a rewrite of dbgen in Java. It generates
//PostgreSQL-compatible esqlc code.

package ohd.hseb.dbgen;

//import ohd.hseb.util.*;

import java.util.*; // needed for List and Map classes and others
import java.util.Date; // needed for Date class, resolves ambiguity with
                       // java.sql.Date
import java.io.*; // needed for file I/O
import java.sql.*;

import ohd.hseb.db.Database;


public class CDbGen
{
    private static final boolean _usingNewDateCode = true;
    private static final boolean _usingNewDateAndTimeCode = false;
    private static final boolean _usingIndicators = true;

    
    //  ---------------------------------------------------------------
    // private data
    //---------------------------------------------------------------

    private Map _cDataTypeIntegerMap = null; //maps from sqlType integer to cDataType String
    private Map _sqlTypeToNullTypeMap = null;
    private Map _keywordMap = null;

    private String _targetDirName = null;

    private DataOutputStream _dos = null;

    private List _tableDescriptorList = null;


    private CodeIndenter _indenter = null;

    private final String _indentString = "    ";
    
    private static final String _sectionSeparator = 
        	"\n/* ------------------------------------------------------------------------- */\n";
    
    
    // -----------------------------------------------------------------------
    public CDbGen()
    {
        initCDataTypeIntegerMap();
        initSqlTypeToNullTypeMap();
        initKeywordMap();
        
    }
    //  -------------------------------------------------------------------------------------
    private void initKeywordMap()
    {
        _keywordMap = new HashMap();
        
        _keywordMap.put("value", "\\\"value\\\"");
        _keywordMap.put("reference", "\\\"reference\\\"");
    }
    
    //  -------------------------------------------------------------------------------------
    
    private void addCDataTypeIntegerMapEntry(int sqlType, String cType)
    {
        _cDataTypeIntegerMap.put(new Integer(sqlType), cType);
    }
  
    //  -------------------------------------------------------------------------------------
    private void initCDataTypeIntegerMap()
    {
        
        _cDataTypeIntegerMap = new HashMap();
        
        //special code handles the difference between internal and external structures
        //for the DATE type, see getCDataTypeByInt()
        addCDataTypeIntegerMapEntry(Types.DATE, "date");  
        
        
        if (_usingNewDateAndTimeCode)
        {
            addCDataTypeIntegerMapEntry(Types.TIMESTAMP, "time_t");
        }
        else
        {
            addCDataTypeIntegerMapEntry(Types.TIMESTAMP, "dtime_t");
        }
        addCDataTypeIntegerMapEntry(Types.REAL, "float");
        addCDataTypeIntegerMapEntry(Types.FLOAT, "double");
        addCDataTypeIntegerMapEntry(Types.DOUBLE, "double");
        addCDataTypeIntegerMapEntry(Types.SMALLINT, "short");
        addCDataTypeIntegerMapEntry(Types.INTEGER, "long");
        addCDataTypeIntegerMapEntry(Types.BIGINT, "long");
        addCDataTypeIntegerMapEntry(Types.LONGVARBINARY, "char *");
        addCDataTypeIntegerMapEntry(Types.LONGVARCHAR, "char *"); // ? instead of byte[]
        addCDataTypeIntegerMapEntry(Types.VARCHAR, "char");
        addCDataTypeIntegerMapEntry(Types.CHAR, "char");
     
			
    }
    //  -------------------------------------------------------------------------------------
    private String wrapInQuotesIfNeeded(String colName, boolean needsEscapes)
    {
        String newColName = colName;
        
        String value = (String) _keywordMap.get(colName);
        
        if (value != null)
        {
            if (needsEscapes)
            {
                newColName = "\\\"" + colName + "\\\"";
            }
            else
            {
                newColName = "\"" + colName + "\"";
            }
        }
        
        return newColName;
    }
    
    //  -------------------------------------------------------------------------------------

    private String getCDataTypeByInt(int sqlType, boolean isPublicType)
    {
        String cDataType = null;
        
        cDataType = (String) _cDataTypeIntegerMap.get(new Integer(sqlType));
        
        //one of few cases in which the public type differs from the internal type
        if (_usingNewDateCode)
        {
            if  ( (sqlType == Types.DATE) && (isPublicType) )
            {
                cDataType = "date_t"; 
            }
        }
        
        if (cDataType == null)
        {
            cDataType = "UNKNOWN_SQL_TYPE_" + sqlType;
        }
        
        return cDataType;
    }
   
    //  -------------------------------------------------------------------------------------
    
    private void addSqlTypeToNullTypeMapEntry(int sqlType, String cType)
    {
        _sqlTypeToNullTypeMap.put(new Integer(sqlType), cType);
    }
    
    //  ------------------------------------------------------------------------------------- 
    private void initSqlTypeToNullTypeMap()
    {
        _sqlTypeToNullTypeMap = new HashMap();
        
        addSqlTypeToNullTypeMapEntry(Types.CHAR, "CHAR");
        addSqlTypeToNullTypeMapEntry(Types.FLOAT, "FLOAT");
        addSqlTypeToNullTypeMapEntry(Types.REAL, "FLOAT");
        addSqlTypeToNullTypeMapEntry(Types.SMALLINT, "SHORT");
        addSqlTypeToNullTypeMapEntry(Types.INTEGER, "INT");
        addSqlTypeToNullTypeMapEntry(Types.BIGINT, "LONG");
        addSqlTypeToNullTypeMapEntry(Types.DOUBLE , "DOUBLE");
        addSqlTypeToNullTypeMapEntry(Types.DATE, "INT"); //not DATE because our public struct is a long
        addSqlTypeToNullTypeMapEntry(Types.TIMESTAMP, "DATETIME");
        addSqlTypeToNullTypeMapEntry(Types.VARCHAR, "CHAR");
        addSqlTypeToNullTypeMapEntry(Types.LONGVARCHAR, "CHAR");
    }
    
    //------------------------------------------------------------------------------------- 

    private String getNullTypeByInt(int sqlType)
    {
        String nullType = null;
        
        nullType = (String) _sqlTypeToNullTypeMap.get(new Integer(sqlType));
        
        
        if (nullType == null)
        {
            nullType = "UNKNOWN_SQL_TYPE_" + sqlType;
        }
        
        return nullType;
    }
   
    // ---------------------------------------------------------------------------------
      
    private void write(String line, boolean useIndentation) throws IOException
    {
        //---------------------------------------------------------------
        // write and writeLine are used to write out nicely indented code
        //---------------------------------------------------------------
      
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
//      ---------------------------------------------------------------
        // write and writeLine are used to write out nicely indented code
        //---------------------------------------------------------------
        write(line, false); //by default, do not use indentation

        return;
    }

    //	 -----------------------------------------------------------------

    private void writeLine(String line) throws IOException
    {
//      ---------------------------------------------------------------
        // write and writeLine are used to write out nicely indented code
        //---------------------------------------------------------------
        boolean useIndentation = true;
        write(line + "\n", useIndentation);

        return;
    }
    
    //	 -----------------------------------------------------------------

    private void writeNewLine() throws IOException
    {
//      ---------------------------------------------------------------
        // write and writeLine are used to write out nicely indented code
        //---------------------------------------------------------------
        boolean useIndentation = false;
        write("\n", useIndentation);

        return;
    }
    // -----------------------------------------------------------------

    private void logError(IOException e)
    {
        System.err.println("Error trying to write to ");
        e.printStackTrace();

        return;
    }


    // --------------------------------------------------------------------------
    // read the list of preferred table names from a text file with the
    // preferred name of the Tables and Views , 1 per line, no spaces in the
    // name
    // --------------------------------------------------------------------------
    private Map getMapOfPreferredTableNames(String filePath)
    {
        Map tableNameMap = new HashMap();

        try
        {

            BufferedReader reader = new BufferedReader(new FileReader(filePath));
            String line = null;

            line = reader.readLine();

            while (line != null)
            {
                // System.out.println("line = " + line + ":");

                String preferredTableName = line.trim();
                String lowercaseTableName = preferredTableName.toLowerCase();

                tableNameMap.put(lowercaseTableName, preferredTableName);

                System.out.println("CDbGen.getMapOfPreferredTableNames(): "
                        + lowercaseTableName + " -> " + preferredTableName);

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

    //  -----------------------------------------------------------------
    private void buildHeaderSrcFiles(String dbName)
    {
        try
        {
            // Strings to hold Table name and File name
            String tableName = new String();
            String fileName = new String();

            // loop through all the table names and build source files
            for (int ctr = 0; ctr < _tableDescriptorList.size(); ctr++)
            {
                TableDescriptor tableDesc = (TableDescriptor) _tableDescriptorList
                        .get(ctr);
                tableName = tableDesc.getName();
              
                fileName = tableName + ".h";

                System.out.println("building source code for " + fileName);

                
                FileOutputStream fos = new FileOutputStream(_targetDirName
                        + "/" + fileName);
                _dos = new DataOutputStream(fos);

                createHeaderFile(tableName, fileName, dbName, tableDesc);

                if ((!tableDesc.isView()) && (tableDesc.hasPrimaryKey()))
                {
                    // createRecordGetWhereString(tableDesc);
                }

                _indenter.setLevel(0);
   
                _dos.close();
            }
        }
        catch (IOException e)
        {
           logError(e);
        }

    } //  end of buildRecordSrcFiles method


    //-----------------------------------------------------------------
    private void createHeaderFile(String tableName, String fileName,
                                  String dbName, TableDescriptor tableDesc)
    {
        try
        {
            _indenter.setLevel(0);
            // current time
            Date currentTime = new Date();

            if (tableDesc.isView())
            {
                writeLine("// This is a view record !");
            }

            writeLine("/*");
            _indenter.incLevel();
            writeLine("File: " + fileName);
            writeLine("Author  : CDBGEN");
            writeLine("Created : " + currentTime + " using database " + dbName);
            writeLine("Description: This header file is associated with its .pgc file ");
            _indenter.incLevel(2);
            writeLine("and defines functions and the table's record structure.");
            _indenter.decLevel(2);
            _indenter.decLevel();
            writeLine("*/");


            writeLine("#ifndef " + tableName + "_h");
            writeLine("#define " + tableName + "_h");
            write("\n\n");
            
            
            writeLine("#include <stdio.h>");
            writeLine("#include <string.h>");
            writeLine("#include <stdlib.h>");
            writeLine("#include <memory.h>");
            writeLine("#include \"DbmsAccess.h\"");
            writeLine("#include \"DbmsUtils.h\"");
            writeLine("#include \"List.h\"");
            writeLine("#include \"GeneralUtil.h\"");
            writeLine("#include \"dbmserrs.h\"");
            writeLine("#include \"datetime.h\"");
            writeLine("#include \"time_convert.h\"");

            writeNewLine();
         
     
            write("\n\n");

            writeLine("typedef struct _" + tableName);
            writeLine("{");
            _indenter.incLevel();

            writeLine("Node\t\tnode;");

            createStructureContents(tableName, tableDesc, true);

            writeLine("List\t\tlist;");
            _indenter.decLevel();
            writeLine("} " + tableName + ";");


            writeLine("/*");
            _indenter.incLevel();
            writeLine("Function Prototypes");
            _indenter.decLevel();
            writeLine("*/");

            _indenter.incLevel();
            writeLine(tableName + "* Get" + tableName + "(const char * where);");
            writeLine(tableName + "* Select" + tableName + "(const char * where);");
            writeLine("int Select" + tableName + "Count(const char * where);");
            
            if (!tableDesc.isView())
            {
                writeLine("int Put" + tableName + "(const " + tableName
                        + " * structPtr);");
                writeLine("int Insert" + tableName + "(const " + tableName
                        + " * structPtr);");
                   
                writeLine("int Update" + tableName + "(const " + tableName +
                          "* structPtr, const char *where);");
                writeLine("int Delete" + tableName + "(const char *where);");
                
                
                if (tableDesc.hasPrimaryKey())
                {
                    writeLine("int Update" + tableName + "ByRecord (const " + tableName +
                            " * newStructPtr, const " + tableName + " * oldStructPtr);");
 
                    writeLine("int InsertOrUpdate" + tableName + "(const " + tableName + " * structPtr);");
                    
                    writeLine("int InsertIfUnique" + tableName + "(const " + tableName + " * structPtr, bool *isUnique);");
                    writeLine("bool " + tableName +"Exists(const " + tableName + " * structPtr);");
                    
                    writeLine("int Delete" + tableName + "ByRecord(const " + tableName + " * structPtr);");
 
                    writeLine("void Get" + tableName + "PrimaryKeyWhereString " + 
                            "(const " + tableName + " * structPtr, char returnWhereString[] );");
                    
                }
            }
            writeLine("void Free" + tableName + "(" + tableName
                    + " * structPtr);");

            writeLine("DbStatus * Get" + tableName + "DbStatus();");
            writeLine("void Set" + tableName +"ErrorLogging(int value);");

            
            _indenter.decLevel();
            writeLine("#endif");
        }
        catch (IOException e)
        {
            logError(e);
        }

    } //  end of createRecordFileBanner method

    //----------------------------------------------------------------------------------------

    private void buildEsqlCSourceFiles(String dbName)
    {
        try
        {
            // Strings to hold Table name and File name
            String tableName = new String();
            String fileName = new String();
            String cursorName = "tcur";

            // loop through all the table names and build source files
            for (int ctr = 0; ctr < _tableDescriptorList.size(); ctr++)
            {
                TableDescriptor tableDesc =
                        (TableDescriptor) _tableDescriptorList.get(ctr);
                
                tableName = tableDesc.getName();

                fileName = tableName + ".pgc";

                System.out.println("building source code for " + fileName);

                FileOutputStream fos = 
                      new FileOutputStream(_targetDirName + "/" + fileName);
                _dos = new DataOutputStream(fos);

                createEsqlCCommentSection(tableName, fileName, dbName,tableDesc);
                createEsqlCIncludeSection(tableName, fileName, dbName);
                createStructSection(tableName, tableDesc);
        
                createMiscDeclarations(tableName);
                writeLine(_sectionSeparator);
                
                  
                createSelectFunction("Get", tableName, tableDesc, cursorName+"1");       
                writeLine(_sectionSeparator);
                
                createSelectFunction("Select", tableName, tableDesc, cursorName+"2");
                writeLine(_sectionSeparator);
                
                createSelectCountFunction("Select", tableName, tableDesc, cursorName+"3");
                writeLine(_sectionSeparator);
                      
                if (!tableDesc.isView())
                {
                    createInsertFunction("Put", tableName, tableDesc);  
                    writeLine(_sectionSeparator);
                    
                    createInsertFunction("Insert", tableName, tableDesc);
                    writeLine(_sectionSeparator);
                    
                    createUpdateFunction(tableName, tableDesc);
                    writeLine(_sectionSeparator);
                               
                    createDeleteFunction(tableName, tableDesc);
                    writeLine(_sectionSeparator);  
                    
                    
                    if (tableDesc.hasPrimaryKey())
                    {          
                        createUpdateByRecordFunction(tableName, tableDesc);
                        writeLine(_sectionSeparator);  
                        
                        createInsertOrUpdateFunction(tableName, tableDesc);
                        writeLine(_sectionSeparator);  
                        
                        createInsertIfUniqueFunction(tableName, tableDesc);
                        writeLine(_sectionSeparator);

                        createDeleteByRecordFunction(tableName, tableDesc);
                        writeLine(_sectionSeparator);
                        
                        createExistsFunction(tableName, tableDesc);
                        writeLine(_sectionSeparator);
                      
                        createGetPrimaryWhereString(tableName, tableDesc);
                        writeLine(_sectionSeparator);
                   }
                    
                }
                  
              
                
                createFreeFunction(tableName, tableDesc);
                writeLine(_sectionSeparator);
                
                createGetDbStatusFunction(tableName);
                writeLine(_sectionSeparator);
                
                createSetErrorLogging(tableName);
                writeLine(_sectionSeparator);

                _indenter.setLevel(0);
     
                _dos.close();
            }
        }
        catch (IOException e)
        {
            logError(e);
     
        }

    } //  end of buildRecordSrcFiles method
    //------------------------------------------------------------------------------------

    private void createEsqlCCommentSection(String tableName, String fileName,
                                           String dbName,
                                           TableDescriptor tableDesc)
    {

        try
        {
            _indenter.setLevel(0);
            // current time
            Date currentTime = new Date();

            if (tableDesc.isView())
            {
                writeLine("/* This is a view record ! */");
            }

            writeLine("/*");
            _indenter.incLevel();
            writeLine("File: " + fileName);
            writeLine("Author  : CDBGEN");
            writeLine("Created : " + currentTime + " using database " + dbName);
            writeLine("Description: This .pgc file has an associated header file. ");
            _indenter.incLevel(2);
            writeLine("and defines all functions and the table's INTERNAL record structure.");
            _indenter.decLevel(2);
            _indenter.decLevel();
            writeLine("*/");
            writeNewLine();
        }
        catch (java.io.IOException e)
        {
            logError(e);
        }

    }

    //----------------------------------------------------------------------------------------

    private void createEsqlCIncludeSection(String tableName, String fileName,
                                           String dbName)
    {
        try
        {
            _indenter.setLevel(0);
            // current time
            Date currentTime = new Date();
            
            writeLine("#include \"" + tableName + ".h\"");
   
     /*
            writeLine("#include <stdio.h>");
            writeLine("#include <string.h>");
            writeLine("#include <stdlib.h>");
            writeLine("#include <memory.h>");
            writeLine("#include \"DbmsAccess.h\"");
            writeLine("#include \"dbmserrs.h\"");
            writeLine("#include \"List.h\"");
      */   
            writeNewLine();
            writeLine("EXEC SQL include sqlda;");
            writeLine("EXEC SQL include sqltypes;");
            writeLine("EXEC SQL include sql3types;"); 
            writeLine("EXEC SQL include pgtypes_timestamp;");
            writeLine("EXEC SQL include pgtypes_date;");
            writeLine("EXEC SQL include pgtypes_interval;");
            writeLine("EXEC SQL include pgtypes_numeric;");
            writeNewLine();
    
        }
        catch (java.io.IOException e)
        {
            logError(e);
        }
        

    }

    //---------------------------------------------------------------------------------------
    private void createStructSection(String tableName, TableDescriptor tableDesc)
    {
        try
        {
            _indenter.setLevel(0);
     
            writeLine("EXEC SQL BEGIN DECLARE SECTION;");
            writeLine("struct " + tableName + "_t");
            writeLine("{");
            _indenter.incLevel();
            createStructureContents(tableName, tableDesc, false);
            _indenter.decLevel();
            writeLine("} " + tableName + "_rec;");
            writeLine("EXEC SQL END DECLARE SECTION;");
            writeNewLine();
        }
        catch (java.io.IOException e)
        {
            logError(e);
        }
    }
  //----------------------------------------------------------------------------------------
    

    private void createMiscDeclarations(String tableName)
    {
        try
        {
            _indenter.setLevel(0);

            writeLine("#define QUERY_LEN 9999");
            writeLine("static int errorLoggingOn = 1;");
            writeNewLine();
            writeLine("static DbStatus dbStatus;");
            writeNewLine();

        }
        catch (java.io.IOException e)
        {
            logError(e);
        }
    }
//  ----------------------------------------------------------------------------------------
    

    private void createSetErrorLogging(String tableName)
    {
         
        try
        {
            _indenter.setLevel(0);

            writeLine("void Set" + tableName +"ErrorLogging(int value)");
            
            writeLine("{");
            _indenter.incLevel();
            
            writeLine("errorLoggingOn = value;");
            writeLine("return;");
            
            _indenter.decLevel();
            writeLine("}");
             
            
            writeNewLine();

        }
        catch (java.io.IOException e)
        {
            logError(e);
        }
    }
//----------------------------------------------------------------------------------------
 
    private void createStructureContents(String tableName,
                                         TableDescriptor tableDesc,
                                         boolean isPublicStructure)
    {

        List columnDescriptorList = tableDesc.getColumnDescriptorList();

        try
        {
            for (int i = 0; i < columnDescriptorList.size(); i++)
            {
                ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);

            
                String columnName = colDesc.getName();
                int sqlTypeInt = colDesc.getSqlTypeInt();
                String cType = getCDataTypeByInt(sqlTypeInt, isPublicStructure);

                String firstChar = String.valueOf(columnName.charAt(0));
             //   String newName = firstChar.toUpperCase()
              //          + columnName.substring(1);

       
                
                if ((sqlTypeInt == Types.CHAR) || (sqlTypeInt == Types.VARCHAR) )
                {
                    int size = colDesc.getSize() + 1;
                    writeLine(cType + "\t\t" + columnName + "[" + size + "];");
                }
                else
                //non-character type
                {
                    writeLine(cType + "\t\t" + columnName + ";");
                }
            } //end for each column
            
            
            if ((_usingIndicators) &&  (! isPublicStructure) ) // declare indicator variables
            {
                writeNewLine();
                for (int i = 0; i < columnDescriptorList.size(); i++)
                {
                    ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                           
                    if (colDesc.isNullable())
                    {
                        String columnName = colDesc.getName();
                        writeLine("int" +"\t\t" + getIndicatorColumnName(columnName) +";");
                    }
              
                } //end for each column
            
            }
        }
        catch (IOException e)
        {
            logError(e);
        }

    } //  end of createRecordCopyConstructor method
    //-------------------------------------------------------------------------------------------
    private String getIndicatorColumnName(String columnName)
    {
        return "ind_" + columnName;    
    }
    //-------------------------------------------------------------------------------------------
    private void createSelectFunction(String functionPrefix,
                                      String tableName,
                                      TableDescriptor tableDesc,
                                      String cursorName)
    {

        try
        {
            _indenter.setLevel(0);
            writeLine(tableName + " * " + functionPrefix + tableName + "(const char * where)");
            writeLine("{");
            _indenter.incLevel();
            
            writeNewLine();
            writeLine(tableName + " * listPtr = NULL;");
            writeLine(tableName + " * structPtr = NULL;");
            writeLine("char selectStatement[] = \"SELECT * FROM " + tableName + " \";");
            writeNewLine();
            
            writeLine("int rowCount = 0;");
            writeLine("int first = 1;");
            writeNewLine();
      
            
            writeLine("EXEC SQL BEGIN DECLARE SECTION;");
            writeNewLine();
            writeLine("struct " + tableName + "_t   dbs;");
            writeLine("char queryBuffer[QUERY_LEN];");
            writeNewLine();
            writeLine("EXEC SQL END DECLARE SECTION;");
            writeNewLine();
            
            writeLine("setDbStatusSqlCommand(&dbStatus, SELECT);");
            writeNewLine();
                
            writeLine("strcpy(queryBuffer, selectStatement);");
            writeNewLine();
            
            writeLine("if ( ( where != NULL ) && ( * where != '\\0' ) ) ");
            
            writeLine("{");
            _indenter.incLevel();
            
            writeLine("strcat(queryBuffer, where);");
            
            _indenter.decLevel();
            writeLine("}");
            writeNewLine();
            
            writeLine("EXEC SQL PREPARE tid FROM :queryBuffer;");
            createErrorChecking(tableName, functionPrefix, "Prepare section", "return (NULL);");
            writeNewLine();
            
            writeLine("EXEC SQL DECLARE " + cursorName + " CURSOR WITH HOLD FOR tid;");
            createErrorChecking(tableName, functionPrefix, "Declare cursor section", "return (NULL);");
            writeNewLine();
        
            writeLine("EXEC SQL OPEN " + cursorName + ";");
            createErrorChecking(tableName, functionPrefix, "Open cursor section", "return (NULL);");
            writeNewLine();
        
            writeLine("listPtr = NULL;");
            writeLine("memset(&dbs, '\\0', sizeof(dbs));");
            writeNewLine();
            
            if (_usingIndicators)
            {
                createFetch(tableDesc, cursorName);
            }
            else
            {
                writeLine("EXEC SQL FETCH " + cursorName + " INTO :dbs;");
            }
            createErrorChecking(tableName, functionPrefix, "Initial FETCH section", "return (NULL);");
            writeNewLine();
            
            writeLine("while (SQLCODE == 0) ");
            writeLine("{");
            _indenter.incLevel();
            
            writeLine("rowCount++;");
            
            writeLine("if ((structPtr = (" + tableName + " *) malloc(sizeof(" + tableName +"))) != NULL)");
            writeLine("{");
            _indenter.incLevel();
            
            writeNewLine();
            createCopyDbStructToCStruct(tableName, tableDesc);
            writeNewLine();
            
            _indenter.decLevel();
            writeLine("}");
            
            writeLine("else");
            writeLine("{");
            _indenter.incLevel();
            writeLine("break;");
            _indenter.decLevel();
            writeLine("}");
            
            writeNewLine();
           
            writeLine("if (first)");
            writeLine("{");
            _indenter.incLevel();
            
            writeLine("listPtr = structPtr;");
            writeLine("ListInit(&listPtr->list);");
            writeLine("first = 0;");
            
            
            _indenter.decLevel();
            writeLine("}"); //end of if (first)
            writeNewLine();
            
            writeLine("ListAdd(&listPtr->list, &structPtr->node);");
            writeLine("memset(&dbs, '\\0', sizeof(dbs));");
            writeNewLine();
            
            if (_usingIndicators)
            {
                createFetch(tableDesc, cursorName);
            }
            else
            {
                writeLine("EXEC SQL FETCH " + cursorName + " INTO :dbs;");
            }
            createErrorChecking(tableName, functionPrefix, "Nth fetch section", "return (NULL);");
            
            
            _indenter.decLevel();
            writeLine("}"); //end of while loop
            writeNewLine();
            
            
            writeLine("initDbStatus(&dbStatus);");
            writeLine("setDbStatusRowsAffected(&dbStatus, rowCount);");
            writeNewLine();
            
            writeLine("EXEC SQL CLOSE " + cursorName + ";");
            writeLine("return(listPtr);");
            
            _indenter.decLevel();
            writeLine("}"); //end of SelectXXX or GetXXX
        
        }
        catch (IOException e)
        {
           logError(e);
        }

    } //  end of createSelectFunction()

    // ----------------------------------------------------------------------------------
    private void createSelectCountFunction(String functionPrefix,
                                      String tableName,
                                      TableDescriptor tableDesc,
                                      String cursorName)
    {

        try
        {
            String functionName = functionPrefix + "Count";
            
            _indenter.setLevel(0);
            writeLine("int " + functionPrefix + tableName + "Count(const char * where)");
            writeLine("{");
            _indenter.incLevel();
            
            writeNewLine();
            writeLine("char selectStatement[] = \"SELECT COUNT(*) FROM " + tableName + " \";");
            writeNewLine();
            
        
            
            writeLine("EXEC SQL BEGIN DECLARE SECTION;");
            writeNewLine();
            writeLine("int rowCount = 0;");    
            writeLine("char queryBuffer[QUERY_LEN];");
            writeNewLine();
            writeLine("EXEC SQL END DECLARE SECTION;");
            writeNewLine();
            
            writeLine("setDbStatusSqlCommand(&dbStatus, SELECT);");
            writeNewLine();
                
            writeLine("strcpy(queryBuffer, selectStatement);");
            writeNewLine();
            
            writeLine("if ( ( where != NULL ) && ( * where != '\\0' ) ) ");
            
            writeLine("{");
            _indenter.incLevel();
            
            writeLine("strcat(queryBuffer, where);");
            
            _indenter.decLevel();
            writeLine("}");
            writeNewLine();
            
            writeLine("EXEC SQL PREPARE tid FROM :queryBuffer;");
            createErrorChecking(tableName, functionName, "Prepare section", "return (-1);");
            writeNewLine();
            
            writeLine("EXEC SQL DECLARE " + cursorName + " CURSOR WITH HOLD FOR tid;");
            createErrorChecking(tableName, functionName, "Declare cursor section", "return (-1);");
            writeNewLine();
        
            writeLine("EXEC SQL OPEN " + cursorName + ";");
            createErrorChecking(tableName, functionName, "Open cursor section", "return (-1);");
            writeNewLine();
        
           
            writeNewLine();
 
            writeLine("EXEC SQL FETCH " + cursorName + " INTO :rowCount;");
            
            createErrorChecking(tableName, functionName, "Initial FETCH section", "return (-1);");
            writeNewLine();
                          
            writeLine("initDbStatus(&dbStatus);");
            writeLine("setDbStatusRowsAffected(&dbStatus, rowCount);");
            writeNewLine();
            
            writeLine("EXEC SQL CLOSE " + cursorName + ";");
            writeLine("return(rowCount);");
            
            _indenter.decLevel();
            writeLine("}"); //end of SelectXXXCount or GetXXXCount
        
        }
        catch (IOException e)
        {
           logError(e);
        }

    } //  end of createSelectCountFunction()
    
    // --------------------------------------------------------------------------------------
    private void createInsertOrUpdateFunction(String tableName,
                                              TableDescriptor tableDesc)
    {
        int dbColumnsPerLine = 3;

        try
        {
            _indenter.setLevel(0);
            writeLine("int InsertOrUpdate" + tableName + "(const " + tableName + " * structPtr)");
            writeLine("{");
            _indenter.incLevel();
                      
            
            writeLine("Update"+ tableName + "ByRecord(structPtr, structPtr);");
            writeLine("setDbStatusSqlCommand(&dbStatus, UPDATE);");
            
            writeNewLine();
            
            writeLine("if ( (SQLCODE < 0) || (SQLCODE == 100) )");
          
            writeLine("{");
            _indenter.incLevel();
            
            writeLine("Insert" + tableName + "(structPtr);");
            writeLine("setDbStatusSqlCommand(&dbStatus, INSERT);");
            		
            
            _indenter.decLevel();
            writeLine("}");
            writeNewLine();
            
            writeLine("initDbStatus(&dbStatus);");
            
            writeLine("return(SQLCODE);");
           
            _indenter.decLevel();
            writeLine("}");
        }
        catch (IOException e)
        {
           logError(e);
        }

    } //  end of createInsertOrUpdateFunction method
    
    // ----------------------------------------------------------------------------------
    private void createExistsFunction(String tableName,
                                      TableDescriptor tableDesc)
    {
        int dbColumnsPerLine = 3;

        try
        {
            _indenter.setLevel(0);
            writeLine("bool " + tableName +"Exists(const " + tableName + " * structPtr)");
            writeLine("{");
            _indenter.incLevel();
                      
            writeLine("int result = false;");
            writeLine("int rowCount = 0;");
            
            writeLine("char whereString[QUERY_LEN];");
            writeNewLine();
            
            writeLine("Get" + tableName + "PrimaryKeyWhereString(structPtr, whereString);");
            writeLine("rowCount = Select" + tableName + "Count(whereString);");
            writeNewLine();
            
            writeLine("if (rowCount > 0)");
            writeLine("{");
            _indenter.incLevel();
            writeLine("result = true;");
            _indenter.decLevel();
            writeLine("}");
            
            writeLine("else");
            writeLine("{");
            _indenter.incLevel();
            writeLine("result = false;");
            _indenter.decLevel();
            writeLine("}");
                         
            writeNewLine();
            writeLine("return(result);");
           
            _indenter.decLevel();
            writeLine("}");
        }
        catch (IOException e)
        {
           logError(e);
        }

    } //  end of createInsertIfUniqueFunction method
    
     // ----------------------------------------------------------------------------------
    private void createInsertIfUniqueFunction(String tableName,
                                              TableDescriptor tableDesc)
    {
        int dbColumnsPerLine = 3;

        try
        {
            _indenter.setLevel(0);
            writeLine("int InsertIfUnique" + tableName + "(const " + tableName + " * structPtr, bool *isUnique)");
            writeLine("{");
            _indenter.incLevel();
                      
            writeLine("int resultCode = 0;");
            
            writeLine("if (" + tableName + "Exists(structPtr))");
            writeLine("{");
            _indenter.incLevel();
            writeLine("setDbStatusSqlCommand(&dbStatus, SELECT);");
            writeLine("*isUnique = false;");
            writeLine("resultCode = dbStatus.sql_code;");
            _indenter.decLevel();
            writeLine("}");
            
            writeLine("else");
            writeLine("{");
            _indenter.incLevel();
            
            writeLine("resultCode = dbStatus.sql_code;");
            
            writeLine("if (resultCode == 0)");
            writeLine("{");
            _indenter.incLevel();
            writeLine("Insert" + tableName + "(structPtr);");
            writeLine("setDbStatusSqlCommand(&dbStatus, INSERT);");
            writeLine("*isUnique = true;");
            writeLine("resultCode = dbStatus.sql_code;");
            
            _indenter.decLevel();
            writeLine("}");
            
            writeLine("else");
            writeLine("{");
            _indenter.incLevel();
            writeLine("*isUnique = false;");	
            _indenter.decLevel();
            writeLine("}");
            
            
            _indenter.decLevel();
            writeLine("}");
             
            writeLine("initDbStatus(&dbStatus);");       
            writeNewLine();
            writeLine("return(resultCode);");
           
            _indenter.decLevel();
            writeLine("}");
        }
        catch (IOException e)
        {
           logError(e);
        }

    } //  end of createInsertIfUniqueFunction method
    
    // ----------------------------------------------------------------------------------
    private void createInsertFunction(String functionPrefix,
                                      String tableName,
                                      TableDescriptor tableDesc)
    {
        int dbColumnsPerLine = 3;

        try
        {
            _indenter.setLevel(0);
            writeLine("int " + functionPrefix + tableName + "(const " + tableName + " * structPtr)");
            writeLine("{");
            _indenter.incLevel();
                      
            writeLine("EXEC SQL BEGIN DECLARE SECTION;");
            writeNewLine();
            writeLine("struct " + tableName + "_t  dbs;");
            writeNewLine();
            writeLine("EXEC SQL END DECLARE SECTION;");
            writeNewLine();
            
            writeLine("setDbStatusSqlCommand(&dbStatus, INSERT);");
            writeNewLine();
            
            writeLine("if (structPtr == NULL)");
            
            writeLine("{");
            _indenter.incLevel();
            
            writeLine("return(ERR_BAD_ARGS);");
            
            _indenter.decLevel();
            writeLine("}");
            writeNewLine();
            
            
            writeLine("memset(&dbs, '\\0', sizeof(dbs));");
            writeNewLine();
            
            createCopyCStructToDbStruct(tableName, tableDesc);
            writeNewLine();
            
            
            if (_usingIndicators)
            {
                createInsertStatement(tableDesc, dbColumnsPerLine);
            }
            else
            {
                writeLine("EXEC SQL INSERT INTO " + tableName + " VALUES " + "(:dbs);");
            }
            
            createFreeTextColumnDbsStructFragment(tableName, tableDesc);
            
            createErrorChecking(tableName, functionPrefix, "Insert section","return (SQLCODE);");
            writeNewLine();
            
          //writeLine("EXEC SQL COMMIT WORK;");
            
            writeLine("initDbStatus(&dbStatus);");
      
            writeLine("return(ERR_OK);");
            
            _indenter.decLevel();
            writeLine("}");
            
        }
        catch (IOException e)
        {
           logError(e);
        }

    } //  end of createInsertFunction method

   
    // ----------------------------------------------------------------------------------
    private void createUpdateFunction(String tableName,
                                      TableDescriptor tableDesc)
    {
         
        _indenter.setLevel(0);
        final int dbColumnsPerLine = 5;
        
        try
        {
            writeLine("int Update" + tableName + "(const " + tableName + " *structPtr, const char *where)");
            
            writeLine("{");
            _indenter.incLevel();
            
            writeLine("EXEC SQL BEGIN DECLARE SECTION;");
            writeNewLine();
            writeLine("struct " + tableName + "_t   dbs;");
            writeLine("char queryBuffer[QUERY_LEN];");
            writeNewLine();
            writeLine("EXEC SQL END DECLARE SECTION;");
            writeNewLine();
            
            writeLine("setDbStatusSqlCommand(&dbStatus, UPDATE);");
            writeNewLine();
             
            createCopyCStructToDbStruct(tableName, tableDesc);
            writeNewLine();
            
            writeLine("sprintf(queryBuffer, \" UPDATE " + tableName + " SET \" );");
                  
            List columnDescriptorList = tableDesc.getColumnDescriptorList();
            int columnCount = columnDescriptorList.size();
           
            //write out the query with ? in it
            write("strcat(queryBuffer, \"", true);
            
            //for each column, put in colName = ?,
            for (int i = 0 ; i < columnCount; i++)
            {
                ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                String colName = colDesc.getName();
                
                colName = wrapInQuotesIfNeeded(colName, true);
                
                write(colName + " = ? ");
                
                if (i < columnCount -1)
                {
                    write(", ");
                    
                    // only put a number of columns on a line at a time
                    // so go to a newline and indent
                    if ((i+1) % dbColumnsPerLine == 0)
                    {
                        writeLine("\");");
                        write("strcat(queryBuffer, \"", true);
                    }
                }
            }     
            write("\");"); //close the strcat statement 
            write("\n");
            writeNewLine();
            
            //append the where clause to the query
            writeLine("if ( (where != NULL) && (*where != '\\0'))");
            writeLine("{");
            _indenter.incLevel();
            writeLine("strcat(queryBuffer, where);");
            _indenter.decLevel();
            writeLine("}");
            writeNewLine();
       
            writeLine("EXEC SQL PREPARE uid FROM :queryBuffer;");
            createErrorChecking(tableName, "Update", "Prepare section", "return(SQLCODE);");
            writeNewLine();
            
            //create the execute statement
            
//          write out the execute statement
            createExecuteUsing(tableDesc, dbColumnsPerLine);
            
            // free any special TEXT column data
            createFreeTextColumnDbsStructFragment(tableName, tableDesc);
            
            
            createErrorChecking(tableName, "Update", "Execute section", "return(SQLCODE);");
            writeNewLine();
           
        //    writeLine("/* need to verify that this works in postgresql! */");
      //      writeLine("num_rows_updated = sqlca.sqlerrd[2];");
            
      //      writeLine("/* why do we have both lines? */");
      //      writeLine("sqlca.sqlerrd[2] = num_rows_updated;");
      
       
      //    writeLine("EXEC SQL COMMIT;");
            writeLine("initDbStatus(&dbStatus);");    
            writeLine("return(ERR_OK);");
          
            _indenter.decLevel();
            writeLine("}");
        }
        catch (IOException e)
        {
            logError(e);
        }
        
    }

    
    // ----------------------------------------------------------------------------------

    private void createUpdateByRecordFunction(String tableName,
                                              TableDescriptor tableDesc)
    {
         
        _indenter.setLevel(0);
        final int dbColumnsPerLine = 5;
        
        try
        {        
            writeLine("int Update" + tableName + "ByRecord (const " + tableName +
                    " * newStructPtr, const " + tableName + " * oldStructPtr)");
            
            _indenter.setLevel(0);
            
            
            writeLine("{");
            _indenter.incLevel();
            
            writeLine("char whereClause[BUFSIZ];");
            
            writeLine("Get" + tableName + "PrimaryKeyWhereString(oldStructPtr, whereClause);");
            
            writeLine("return (Update" + tableName + "(newStructPtr, whereClause));");
            
            _indenter.decLevel();
            writeLine("}");       
        }
        catch (IOException e)
        {
            logError(e);
        }
    }
    // ----------------------------------------------------------------------------------
    private void createDeleteFunction(String tableName,
                                      TableDescriptor tableDesc)
    {   
        _indenter.setLevel(0);
        
        try
        {
            writeLine("int Delete" + tableName + "(const char *where)");
            writeLine("{");
            _indenter.incLevel();
            
            writeLine("char deleteStatement[] = \"DELETE FROM " + tableName + " \";");
            
            writeLine("EXEC SQL BEGIN DECLARE SECTION;");
            writeNewLine();
            writeLine("char queryBuffer[QUERY_LEN];");
            writeNewLine();
            writeLine("EXEC SQL END DECLARE SECTION;");
            writeNewLine();
            
            writeLine("strcpy(queryBuffer, deleteStatement);");
            
//          append the where clause to the query
            writeLine("if ( (where != NULL) && (*where != '\\0'))");
            writeLine("{");
            _indenter.incLevel();
            writeLine("strcat(queryBuffer, where);");
            _indenter.decLevel();
            writeLine("}");
            
            writeLine("EXEC SQL EXECUTE IMMEDIATE :queryBuffer;");
            createErrorChecking(tableName, "Delete", "Execute Immediate section", "return(SQLCODE);");
            writeNewLine();
            
            writeLine("initDbStatus(&dbStatus);");  
            writeLine("return(ERR_OK);");
            
            _indenter.decLevel();
            writeLine("}"); 
            
        }
        catch (IOException e)
        {
            logError(e);
        }
    }
    // ----------------------------------------------------------------------------------
    
    private void createDeleteByRecordFunction(String tableName,
                                      TableDescriptor tableDesc)
    {
    
        _indenter.setLevel(0);
        
        try
        {
            writeLine("int Delete" + tableName + "ByRecord(const " + tableName + " * structPtr)");

            writeLine("{");
            _indenter.incLevel();
            
            writeLine("char whereClause[BUFSIZ];");
            
            writeLine("Get" + tableName + "PrimaryKeyWhereString(structPtr, whereClause);");
            
            writeLine("return (Delete" + tableName + "(whereClause));");
             
            _indenter.decLevel();
            writeLine("}");
          
            
        }
        catch (IOException e)
        {
            logError(e);
        }
    }
    // ----------------------------------------------------------------------------------
    private void createFreeTextColumnStructFragment(String tablename, TableDescriptor tableDesc)
    {
        
        List columnDescriptorList = tableDesc.getColumnDescriptorList();
        
        
        try
        {
            
            // do a special free for any TEXT fields
            for (int i = 0; i < columnDescriptorList.size(); i++)
            {
                ColumnDescriptor columnDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                
                if (columnDesc.getSqlTypeInt() == Types.LONGVARCHAR)
                {
                    writeLine("if (structPtr->" + columnDesc.getName() + " != NULL)");
                    writeLine("{");
                    _indenter.incLevel();
                    
                    writeLine("free(structPtr->" + columnDesc.getName() + ");");
                    
                    _indenter.decLevel();
                    writeLine("}"); 
                }
            }
            
        }
        catch (IOException e)
        {
            logError(e);
        }
    }
    
    // ----------------------------------------------------------------------------------
    
    private void createFreeTextColumnDbsStructFragment(String tablename, TableDescriptor tableDesc)
    {
        
        List columnDescriptorList = tableDesc.getColumnDescriptorList();
        
        try
        {  
            // do a special free for any TEXT fields
            for (int i = 0; i < columnDescriptorList.size(); i++)
            {
                ColumnDescriptor columnDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                
                if (columnDesc.getSqlTypeInt() == Types.LONGVARCHAR)
                {
                    writeLine("if (dbs." + columnDesc.getName() + " != NULL)");
                    writeLine("{");
                    _indenter.incLevel();
                    
                    writeLine("free(dbs." + columnDesc.getName() + ");");
                    
                    _indenter.decLevel();
                    writeLine("}"); 
                }
            }
            
        }
        catch (IOException e)
        {
            logError(e);
        }
    }
    
    // ----------------------------------------------------------------------------------
    private void createFreeFunction(String tableName, TableDescriptor tableDesc)                        
    {
        _indenter.setLevel(0);
        
        List columnDescriptorList = tableDesc.getColumnDescriptorList();
        
        
        
        try
        {

            writeLine("void Free" + tableName + "( " + tableName
                    + " * structPtr)");
            writeLine("{");
            _indenter.incLevel();

            writeLine(tableName + "* nextPtr = NULL;");
            writeNewLine();

            writeLine("while (structPtr != NULL)");
            writeLine("{");
            _indenter.incLevel();

            writeLine("nextPtr = ( " + tableName
                    + " * ) ListNext ( &structPtr->node );");
            
            
//          do a special free for any TEXT fields
            createFreeTextColumnStructFragment(tableName, tableDesc);
            
 /*           
            // do a special free for any TEXT fields
            for (int i = 0; i < columnDescriptorList.size(); i++)
            {
                ColumnDescriptor columnDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                
                if (columnDesc.getSqlTypeInt() == Types.LONGVARCHAR)
                {
                    writeLine("if (structPtr->" + columnDesc.getName() + " != NULL)");
                    writeLine("{");
                    _indenter.incLevel();

                    writeLine("free (structPtr->" + columnDesc.getName() + ");");
                    
                    _indenter.decLevel();
                    writeLine("}"); 
                }
            }
 */           
            writeLine("free (structPtr);");
            writeLine("structPtr = nextPtr;");

            _indenter.decLevel();
            writeLine("}"); //close the while loop

            writeLine("return;");

            _indenter.decLevel();
            writeLine("}"); // close the function

        }
        catch (IOException e)
        {
            logError(e);
        }
    }
    // ----------------------------------------------------------------------------------
    private void createGetDbStatusFunction(String tableName)
    {
        _indenter.setLevel(0);
        
        try
        {
            writeLine("DbStatus * Get" + tableName + "DbStatus()");
            writeLine("{");
            _indenter.incLevel();
            
            writeLine("return &dbStatus;");
            
            _indenter.decLevel();
            writeLine("}"); // close the function
        }
        
        catch (IOException e)
        {
            logError(e);
        }
    
    }
    // ----------------------------------------------------------------------------------
    private List getKeyColumnDescriptorList(TableDescriptor tableDesc)
    {
        List keyColDescList = new ArrayList();
        
        List columnDescriptorList = tableDesc.getColumnDescriptorList();
        int columnCount = columnDescriptorList.size();
         
//      for each key column, check if it is a key column, and, if so, then insert it into the list
        for (int i = 0 ; i < columnCount; i++)
        {
            ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);
            
            if (colDesc.isKeyColumn())
            {
                keyColDescList.add(colDesc);
            }
        }
        
        return keyColDescList;
    }
    // ----------------------------------------------------------------------------------
    private int getDateColumnCount(List columnDescriptorList)
    {
        
        int dateColumnCount = 0;
        int columnCount = columnDescriptorList.size();
        
        for (int i = 0 ; i < columnCount; i++)
        {
            ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);
            
            if (colDesc.getSqlTypeInt() == Types.DATE)
            {
                dateColumnCount++;
            }
        }
        
        return dateColumnCount;
    }
    
    // ----------------------------------------------------------------------------------
    private int getTimeColumnCount(List columnDescriptorList)
    {
        
        int timeColumnCount = 0;
        int columnCount = columnDescriptorList.size();
        
        for (int i = 0 ; i < columnCount; i++)
        {
            ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);
            
            if (colDesc.getSqlTypeInt() == Types.TIMESTAMP)
            {
                timeColumnCount++;
            }
        }
        
        return timeColumnCount;
    }
    
    // ----------------------------------------------------------------------------------

    
    private void createGetPrimaryWhereString(String tableName, TableDescriptor tableDesc)
    {
        /*
           generate the following type of statement
           sprintf(returnWhereString, "WHERE lid = '%s' AND pe  = '%s' AND dur = %d AND ts = '%s' AND extremum = %d AND obstime = '%s' ",
			 structPtr->lid, structPtr->pe, structPtr->dur,
			 structPtr->ts, structPtr->extremum, structPtr->obsTime);
  
         */
        String header = "CDbGen.createGetPrimaryWhereString(): ";
        final int dbColumnsPerLine = 5;
        _indenter.setLevel(0);

        int dateColumnCount = 0;
        int timeColumnCount = 0;
        
        try
        {

            writeLine("void Get" + tableName + "PrimaryKeyWhereString " + "(const " + tableName + " * structPtr, char returnWhereString[] )");
            writeLine("{");
            _indenter.incLevel();

            List keyColumnDescriptorList = tableDesc.getKeyColumnDescriptorList();
            int columnCount = keyColumnDescriptorList.size();
            
            
            dateColumnCount = getDateColumnCount(keyColumnDescriptorList);
            timeColumnCount = getTimeColumnCount(keyColumnDescriptorList);
            
            for (int i = 0; i < dateColumnCount; i++)
            {
                writeLine("char date_buffer" + i + "[40];");
            }
            
            
            for (int i = 0; i < timeColumnCount; i++)
            {
                writeLine("char time_buffer" + i + "[40];");
            }
            
            write("sprintf(returnWhereString, \"WHERE ", true);
            
//          for each key column, insert the lid = '%s' part of the sprintf statement
            for (int i = 0 ; i < columnCount; i++)
            {
                ColumnDescriptor colDesc = (ColumnDescriptor) keyColumnDescriptorList.get(i);
                int sqlType = colDesc.getSqlTypeInt();
                
                
                switch (sqlType)
                {
                    case Types.CHAR:
                    case Types.VARCHAR:
                    {
                        write(colDesc.getName() + " = '%s' " );
                        break;
                    }
                    case Types.SMALLINT:
                    {
                        write(colDesc.getName() + " = %d ");
                        break;
                    }
                    case Types.INTEGER:
                    {
                        write(colDesc.getName() + " = %ld ");
                        break;
                    }
                    
                    case Types.FLOAT:
                    case Types.REAL:
                    {
                        write(colDesc.getName() + " = %f ");
                        break;
                    }
                    
                   
                    case Types.DOUBLE:
                    {
                        write(colDesc.getName() + " = %lf ");
                        break;
                    }
                    
                    case Types.DATE:
                    {
                        //this case will have a corresponding
                        //conversion to SQL string from date
                         
                        if (_usingNewDateCode)
                        {
                            write(colDesc.getName() + " = '%s' ");
                        }
                        else
                        {
                            write(colDesc.getName() + " = '%ld' ");
                        }
                        break;
                    }
                        
                    case Types.TIMESTAMP:
                    {
                      
                        //this case will have a corresponding
                        //conversion to SQL string from timestamp
                        write(colDesc.getName() + " = '%s' ");
                   
                        break;
                    }
                    default:
                    {
                        System.out.println(header + "type = " + sqlType);
                        break;
                    }
                    
                }       		
                
                
                //if not the last column
                if (i < columnCount - 1)
                {
                    write(" AND ");   
                }
                else //last one before parameter list
                {
                    write("\",\n");
                }
                
                
            }     
            
            
            //indent 3 times
            write("", true);
            write("", true);
            write("", true);
            
            //for each key column, insert the ,sPtr->lid, sPtr->pe 
            int dateBufferCount = 0;
            int timeBufferCount = 0;
            
            for (int i = 0 ; i < columnCount; i++)
            {
                ColumnDescriptor colDesc = (ColumnDescriptor) keyColumnDescriptorList.get(i);
                int sqlType = colDesc.getSqlTypeInt();
                 
                switch (sqlType)
                {
                    case Types.DATE:
                    {
                        //this case will have a corresponding
                        //conversion to SQL string from timestamp
                        if (_usingNewDateCode)
                        {
                           
                            write("date_t_to_ansi_date(structPtr->" + colDesc.getName()+ ", date_buffer"+ dateBufferCount + ")");
                            dateBufferCount++;
                        }
                        else
                        {
                            write("structPtr->" + colDesc.getName());
                        }
                        break;
                    }
                    case Types.TIMESTAMP:
                    {
                        //this case will have a corresponding
                        //conversion to SQL string from timestamp
                        if (_usingNewDateAndTimeCode)
                        {
                            write("timet_to_ansi(structPtr->" + colDesc.getName() + ", time_buffer" + timeBufferCount + ")");
                            timeBufferCount++;
                        }
                        else
                        {   
                            write("dtimet_to_ansi(structPtr->" + colDesc.getName() + ", time_buffer" + timeBufferCount + ")");
                            timeBufferCount++;
                        }
                        break;
                    }
                    default:
                    {
                        write("structPtr->" + colDesc.getName());
                        break;
                    }
                    
                }       		
                
                
                //if not the last column
                if (i < columnCount - 1)
                {
                    write(", ");

                    if ((i+1) % dbColumnsPerLine == 0)
                    {
                        write("\n");
                        write("", true);
                        write("", true);
                        write("", true);
                    }
                    
                }
                
            }     
          
            
            write(");\n"); //close sprintf statement
            
            
            _indenter.decLevel();
            writeLine("}");
   

        }
        catch (IOException e)
        {
            logError(e);
        }
        
    }
    // ----------------------------------------------------------------------------------
    
    private void createCopyDbStructToCStruct(String tableName, TableDescriptor tableDesc)
    {
        
        int columnCount = tableDesc.getColumnDescriptorList().size();
        List columnDescriptorList = tableDesc.getColumnDescriptorList();
         
        try
        {
            for (int i = 0; i < columnCount; i++)
            {
                ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                
                String colName = colDesc.getName();
                String indName = getIndicatorColumnName(colName);
                
                int sqlType = colDesc.getSqlTypeInt();
                boolean isNullable = colDesc.isNullable();
                
                String indicatorType = getNullTypeByInt(sqlType);
            
                switch(sqlType)
                {
                    case Types.CHAR:
                    case Types.VARCHAR:
                    {
                        writeLine("strcpy(structPtr->" + colName +
                                ", dbs." + colName + ");");
                        break;
                    }
                    case Types.LONGVARCHAR:
                    {
                          
                        //make sure I free any automatically allocated space in the dbs structure
                        writeLine("if (dbs." + colName + " != NULL)");
                        writeLine("{");
                        _indenter.incLevel();
                        
                        writeLine("structPtr->" + colName +
                                " = malloc(strlen(dbs." + colName + ") + 1);") ;
                        writeLine("memset(structPtr->" + colName + ", '\\0', strlen(dbs." + colName + ") +1); ");       
                        writeLine("strcpy(structPtr->" + colName +
                                ", dbs." + colName + ");");
                     
                        writeLine("free(dbs." + colName +");");
                       
                        _indenter.decLevel();
                        writeLine("}");
                     
                        
                        break;
                    }
                    
                    case Types.DATE:
                    {
                        if (_usingNewDateCode)
                        {
                            writeLine("structPtr->" + colName +
                                    " = pg_date_to_date_t(dbs." + colName + ");");
                        }
                        else
                        {
                            writeLine("structPtr->" + colName +
                                    " = dbs." + colName + ";");
                        }
                        break;
                    }    
                    
                    
                    case Types.TIMESTAMP:
                    {
                        if (_usingNewDateAndTimeCode)
                        {
                            writeLine("structPtr->" + colName +
                                    " = timestamp_to_timet(dbs." + colName + ");");
                        }
                        else
                        {
                            writeLine("structPtr->" + colName +
                                    " = dbs." + colName + ";");
                        }
                        break;
                    }
                     
                    default:
                    {
                        writeLine("structPtr->" + colName +
                                " = dbs." + colName + ";");
                    }
                    
                } //end switch
                
                if (_usingIndicators)
                {  
                    String reference = ""; //puts in a "&" if not a string      
                    if (! indicatorType.equals("CHAR"))
                    {
                        reference = "&";
                    }
                    
                    if (isNullable)
                    {     
                        writeLine("setNullIfIndicated(dbs." + indName + 
                                ", " + indicatorType +", " + reference + "structPtr->" + colName +");");
                    }
                    
                    //space out the copying so that pairs of setNullIfIndicated and
                    // data copying are placed together for each column
                    writeNewLine(); 
                }
                
            } //end for
            
        } //end try
        catch (IOException e)
        {
            logError(e);
        }
        return;
    }
    
    // ----------------------------------------------------------------------------------
    private void createExecuteUsing(TableDescriptor tableDesc, int dbColumnsPerLine)
    { 
        try
        {        
            List columnDescriptorList = tableDesc.getColumnDescriptorList();
            
            int size = columnDescriptorList.size();
            write("EXEC SQL EXECUTE uid USING ", true);
            
            //for each column, substitute,
            for (int i = 0 ; i < size; i++)
            {
                ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                
                String colName = colDesc.getName();
           
                String indName = getIndicatorColumnName(colName);
                
                if (_usingIndicators)
                {
                    if (colDesc.isNullable())
                    {
                        write(" :dbs." + colName + ":dbs." + indName);     
                    }
                    else
                    {
                        write(" :dbs." + colName);    
                        
                    }
                }
                else
                {
                    write(" :dbs." + colName);
                }
                
                //if not the last column
                if (i < size -1)
                {
                    write(",");
                    
                    //only put a number of columns on a line at a time
                    // so go to a newline and indent
                    if ( (i+1) % dbColumnsPerLine == 0)
                    {
                        write("\n");     // newline
                        write("", true); //indent again
                    }
                }
            }     
            write(";\n"); //close the strcat statement 
            writeNewLine();  
        }
        catch (IOException e)
        {
            logError(e);
        }
    }
    
    // ----------------------------------------------------------------------------------
    private void createInsertStatement(TableDescriptor tableDesc, int dbColumnsPerLine)
    { 
        final int numColumnsPerLine = 3;
        
        try
        {        
            String tableName = tableDesc.getName();
            List columnDescriptorList = tableDesc.getColumnDescriptorList();
            
            int size = columnDescriptorList.size();
            write("EXEC SQL INSERT INTO " + tableName + " (", true);
            
            _indenter.incLevel();
            
            for (int i = 0 ; i < size; i++)
            {
                ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                
                String colName = colDesc.getName();
                String indName = getIndicatorColumnName(colName);
                
                colName = wrapInQuotesIfNeeded(colName, false);
                
                write(colName);
                
                //if not the last column
                if (i < size -1)  
                {
                    
                   write(",");
                   if ( ((i-1) % numColumnsPerLine) == 0)
                   {
                       // put in a new line
                       writeNewLine();
                       
                       // indent
                       write("", true);
                   }
                }
                else //last column
                {
                    //put in the closing parenthesis
                    writeLine(")");
                }
            }
            
            writeLine("VALUES (");
            
            //for each column, substitute,
            for (int i = 0 ; i < size; i++)
            {
                ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                
                String colName = colDesc.getName();
                String indName = getIndicatorColumnName(colName);
                
                if(i == 0)
                {
                    write("", true);  //make it indent
                }
                
                if (_usingIndicators)
                {
                    if (colDesc.isNullable())
                    {
                        write(" :dbs." + colName + ":dbs." + indName);     
                    }
                    else
                    {
                        write(" :dbs." + colName);    
                        
                    }
                }
                else
                {
                    write(" :dbs." + colName);
                }
                
                //if not the last column
                if (i < size -1)
                {
                    write(",");
                    
                    //only put a number of columns on a line at a time
                    // so go to a newline and indent
                    if ( (i+1) % dbColumnsPerLine == 0)
                    {
                        write("\n");     // newline
                        write("", true); //indent again
                    }
                }
            }     
            write(");\n"); //close VALUES phrase 
            writeNewLine();  
            
            
            _indenter.decLevel();
        }
        catch (IOException e)
        {
            logError(e);
        }
    }
    // ----------------------------------------------------------------------------------

    private void createFetch(TableDescriptor tableDesc, String cursorName)
    {
        List columnDescriptorList = tableDesc.getColumnDescriptorList();
       
        int size = columnDescriptorList.size();
        
        try
        {
            
            writeLine("EXEC SQL FETCH " + cursorName + " INTO ");
            
            for (int i = 0; i < size; i++)
            {
                ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                String colName = colDesc.getName();
                String indName = getIndicatorColumnName(colName);
                boolean isNullable = colDesc.isNullable();
                
                if (i == 0)
                {
                    write("", true);
                }
                
                if (isNullable)
                {
                    write(":dbs." + colName + ":dbs."+indName);
                }
                else
                {
                    write(":dbs." + colName);
                }
                
                //if not last column
                if (i < size - 1)
                {
                   // add comma
                   write(", ", false); 
                   
                   // every 2 columns, start using a new line
                   // and then indent in anticipation of next line
                   if ( ( (i-1) % 2) == 0)
                   {
                      writeNewLine();
                      write("", true);
                   }
                }
                else //last column
                {
                   writeLine(";");
                   writeNewLine();
                }
                
            } //end for
            
        }
        catch (IOException e)
        {
            logError(e);
        }
       
    }
    // ----------------------------------------------------------------------------------
    private void createCopyCStructToDbStruct(String tableName, TableDescriptor tableDesc)
    {
        
        int columnCount = tableDesc.getColumnDescriptorList().size();
        List columnDescriptorList = tableDesc.getColumnDescriptorList();
        
        try
        {
            
            for (int i = 0; i < columnCount; i++)
            {
                ColumnDescriptor colDesc = (ColumnDescriptor) columnDescriptorList.get(i);
                int sqlType = colDesc.getSqlTypeInt();
                String colName = colDesc.getName();
                String indName = getIndicatorColumnName(colName);
                boolean isNullable = colDesc.isNullable();
                
                String indicatorType = getNullTypeByInt(sqlType);
                
                switch(sqlType)
                {
                    case Types.CHAR:
                    case Types.VARCHAR:
                    {
                        writeLine("strcpy(dbs." + colName +
                                ", structPtr->" + colName + ");");
                        
                       
                        
                        break;
                    }
                    
                    //text field
                    case Types.LONGVARCHAR:
                    {
                        
                        writeLine("dbs." + colName +
                                " = malloc(strlen(structPtr->" + colName + ") + 1);") ;
                        writeLine("memset(dbs." + colName + ", '\\0', strlen(structPtr->" + colName + ") +1); ");            
                        writeLine("strcpy(dbs." + colName +
                                ", structPtr->" + colName + ");");
                        
                        
                        break;
                    }
                    
                    case Types.DATE:
                    {
                        if (_usingNewDateCode)
                        {
                            writeLine("dbs." + colName +
                                    " = date_t_to_pg_date(structPtr->" + colName + ");");
                        }
                        else
                        {
                            writeLine("dbs." + colName +
                                    " = structPtr->" + colName + ";");  
                        }
                         
                        break;
                    }    
                    
                    
                    case Types.TIMESTAMP:
                    {
                        if (_usingNewDateAndTimeCode)
                        {
                            writeLine("dbs." + colName +
                                    " = timet_to_timestamp(structPtr->" + colName + ");");
                        }
                        else
                        {
                            writeLine("dbs." + colName +
                                    " = structPtr->" + colName + ";"); 
                        }
                        
                        
                        break;
                    }
                    
                   
                    
                    default:
                    {
                        writeLine("dbs." + colName +
                                " = structPtr->" + colName + ";");
                        break;
                    }
                    
                } //end switch
                
                //add the indicator checking
                if (_usingIndicators)
                {
                    
                    String reference = ""; //puts in a "&" if not a string
                    
                    if (! indicatorType.equals("CHAR"))
                    {
                        reference = "&";
                    }
                    
                    if (isNullable)
                    {
                        writeLine("dbs." + indName + 
                                " = getIndicator(" + 
                                indicatorType + 
                                ", (void *)" + reference + "structPtr->" + colName + ");");
                    }
                    
                    //space out the copying so that pairs of setNullIfIndicated and
                    // data copying are placed together for each column
                    writeNewLine();                
                }
                
            } //end for
            
            
            
        } //end try
        
        catch (IOException e)
        {
            logError(e);
        }
        
        
        
        return;
    }
    // ----------------------------------------------------------------------------------
    private void createErrorChecking(String tableName, String functionName, String functionSection, String returnValueString)
    {
        try
        {
            writeLine("if (SQLCODE < 0)");
            writeLine("{");
            _indenter.incLevel();
            
            
            writeLine("if (errorLoggingOn)");
            writeLine("{");
            _indenter.incLevel();
  
            writeLine("fprintf(stderr, \"" + functionName + tableName + "() in " + functionSection + " --- ERROR\\n\");");
            writeLine("fprintf(stderr, \"SQLCODE = (%ld) sql state = (%s)\\n\", SQLCODE, sqlca.sqlstate);");   
            writeLine("fprintf(stderr, \"Error Message (%s)\\n\", sqlca.sqlerrm.sqlerrmc);");
            writeLine("fflush(stderr);");
            
            _indenter.decLevel();
            writeLine("}");
            
            writeLine("initDbStatus(&dbStatus);");
            
            if (returnValueString != null)
            {
                writeLine(returnValueString);
            }
            
            _indenter.decLevel();
            writeLine("}");
    
       
        }
        catch(IOException e)
        {
            logError(e);
        }
    }
    // ----------------------------------------------------------------------------------
    

    public void generate(String connectionURL, String preferredTableNameFilePath,
                         String dbName, String targetDir, String driverName)
    {


        //		define an instance of a the IHFS database class
        Database currentDatabase = new Database();

        if (driverName != null)
        {
            currentDatabase.setDriverClassName(driverName);
        }

        // call the method to connect to the database
        currentDatabase.connect(connectionURL);

   
        //store the path of the target directory for the generated files
        _targetDirName = targetDir;

        //reads a list of Preferred table names from the directory indicated by
        //schemaDirName
        Map preferredTableNameMap = 
            getMapOfPreferredTableNames(preferredTableNameFilePath);

        // returns a list of the TableDescriptors which contain metadata about
        // the tables and columns. 
        SchemaDescriber finder = new SchemaDescriber(currentDatabase);


        _tableDescriptorList = finder.getTableDescriptorList(preferredTableNameMap);


        _indenter = new CodeIndenter(_indentString);

        //generate all the XXXRecord.java files
        buildHeaderSrcFiles(dbName);

        //generate all the XXXTable.jar files
        buildEsqlCSourceFiles(dbName);

        // disconnect from the database
        currentDatabase.disconnect();

    } //end generate

    //---------------------------------------------------------------
    // 
    //---------------------------------------------------------------
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
            System.err.println("Usage:  java CDbGen connectionURL preferredTableNameFilePath dbName targetDir driverClassName");
        }
        else
        // the argument count looks good
        {
            connectionURL = args[0];
            preferredTableNameFilePath = args[1];

            // the database name is passed in as the third command line argument
            dbName = args[2];

            targetDir = args[3];
      
            driverName = args[4];
            
            // create a JDbGen object named dbgen
            CDbGen dbgen = new CDbGen();

            dbgen.generate(connectionURL, preferredTableNameFilePath, dbName, targetDir, driverName);

            System.out.println("CDBGEN completed!");
            //timer.stop("CDbGen took");
        }

        return;

    } // end of main

} // end of  JDbGen class
