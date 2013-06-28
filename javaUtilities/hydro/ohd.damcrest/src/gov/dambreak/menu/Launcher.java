package gov.dambreak.menu;

import java.io.File;
import javax.swing.*;

import gov.damcat.data.DBAccess;
import gov.damcat.data.DamInfo;
import gov.dambreak.util.AnalysisData;
import gov.dambreak.smpdbk.ModelGUI;
import gov.dambreak.smpdbk.OutputManager;
import gov.damcat.data.Search;

/**
 * This class represents the starting point for the Dambreak
 * Analysis application.
 * Creation date: (7/30/2003 2:22:34 PM)     
 * @author: 
 */
public class Launcher 
{
    private boolean bDBMSActive;
    public static boolean closeFlag = false;
    
    /**
     * Make calls to read database, and process dam with nidid passed in via command line
     * Creation date: (4/1/2004 10:43:09 AM)
     */
    private void connOutMan(String nidid) 
    {
        boolean hasDatabase;
        closeFlag = true;
        
        DBAccess dbAccess = new gov.damcat.data.DBAccess();
        
        DamInfo damInfo = dbAccess.fillCompletely(nidid);
        
        AnalysisData currentData = damInfo.getAnalysisData();
        
        String oError = currentData.verifyOutput();
        
        if( !oError.equalsIgnoreCase( "" ) )
        {
            hasDatabase = true;
            
            JOptionPane.showMessageDialog(null,oError,"Could Not Display Stored Forecast",JOptionPane.ERROR_MESSAGE);
            ModelGUI smpdbkInputEditor = new ModelGUI( hasDatabase, damInfo.nidid,damInfo.getAnalysisData(),dbAccess);
            smpdbkInputEditor.handleMenuRun_SMPDBK();
            
            //hasSpawned = true;  //???
            
            return;
        }
        
        OutputManager outMan = new OutputManager(currentData);
        //closeFlag = false;  //???
    }
    /**
     * Insert the method's description here.
     * Creation date: (10/3/2003 5:20:08 PM)
     * @return boolean
     */
    public boolean isDBMSActive() 
    {
        return bDBMSActive;
    }
    /**
     * This is the starting point of the Dambreak Analysis application.
     * Creation date: (7/30/2003 2:22:39 PM)
     * @param args java.lang.String[]
     */
    public static void main(String[] args) 
    {
        //////////////////////////////////////////////////////////////////////////////////
        // Dambreak Analysis v1.0 - Craig Austin
        //                   v2.0 - Gang Of Five : Campbell, Hsu, Momo, Shebsovich, Urban 
        //					 v3.0 - Dan Urban & Moria Shebsovich
        /////////////////////////////////////////////////////////////////////////////////
        
        Launcher launcher = new Launcher(); 
        
        System.out.println( "Initializing Dambreak Analysis..." );
        
        boolean bHasModel = false;
        
        String filePath = "";
        String modelFileName = "";
        
        String osName = System.getProperty( "os.name" );
        String osNameLowerCase = osName.toLowerCase();
        
        // *** First, find damcrest.home to determine where the resources.txt and sdbj.exe files are located
        
        if ( ( filePath = System.getProperty( "damcrest.home" ) ) == null )
        {
            JOptionPane.showMessageDialog(null,"Error -- damcrest.home property missing\n", "Error", JOptionPane.ERROR_MESSAGE);
            return;
        }
        if ( !filePath.endsWith( System.getProperty("file.separator" ) ) )
        {
            filePath = filePath.concat( System.getProperty("file.separator" ) );
        }
        
        String resourcesFileName = filePath.concat( "resources.txt" );
        File fileresources = new File( resourcesFileName );
        System.out.println( "osNameLowerCase = " + osNameLowerCase );
        if( osNameLowerCase.indexOf( "linux" ) > -1 ) 
        {
            if ( !fileresources.exists() )
            {
                System.out.println ("OS Linux, all settings are taken from tokens or environmental variables\n");
            }
            else
            {
                System.out.println ("OS Linux, resourses.txt esists\n");
            }
        }
        else
        {
            if ( !fileresources.exists() ) 
            {
                JOptionPane.showMessageDialog(null,"Error -- no/bad resources.txt file found in\n" + filePath, "Error", JOptionPane.ERROR_MESSAGE);
                return;
            }
        }
        if ( osNameLowerCase.indexOf( "windows" ) > -1 ) 
        {
            modelFileName = filePath.concat("sdbj.exe");		// *** is this the same in Linux ???
        }
        else if (osNameLowerCase.indexOf("linux") > -1) 
        {
            modelFileName = filePath.concat("sdbj.LX");
        }
        
        File fileModel = new File(modelFileName);
        if ( fileModel.exists() )
        {
            bHasModel = true;
        }
        
        // *** Next, make sure that we have access to the .jar file with the JClass libraries
        
        boolean bHasJClasses = false;
        try 
        {
            Class.forName("com.klg.jclass.chart.data.JCEditableDataSource");
            Class.forName("com.klg.jclass.chart.JCChart");
            bHasJClasses = true;
        } 
        catch (ClassNotFoundException ex) 
        {
            bHasJClasses = false;
        }
        
        // *** And, the .jar file with necessary FLDAT components
        
        boolean bHasFLDAT = false;
        try 
        {
            Class.forName("gov.noaa.FloodWav.Component.ElevationStationManager");
            bHasFLDAT = true;
        } 
        catch (ClassNotFoundException ex) 
        {
            bHasFLDAT = false;
        }
        
        // *** Now, find out if the user has a database.
        boolean bHasDatabase = DBAccess.isDBEnabled();
        // set private static variable so everyone can find the value 
        launcher.setDBMSActive( bHasDatabase );
        
        // *** If the variable "damcat.hasListAllDams is set in resource.txt file,
        // the list of all dams in the databse will be displayed 
        // when the Search tool is opened initially
        boolean bHasListAllDams = DBAccess.hasListAllDams();
        
        // We have all the data we need, decide where to start the user and alert them of
        // 	any missing components.
        
        if ( bHasDatabase )
        {
            System.out.println( "User has DAMCAT." );
        }
        else
        {
            System.out.println("User does not have DAMCAT.");
        }
        
        if ( osNameLowerCase.indexOf( "windows" ) > -1) 
        {
            if (bHasModel)
            {
                System.out.println( "Found sdbj.exe." );
            }
            else
            {
                System.out.println("!!! Could not find sdbj.exe !!!");
            }
        }
        
        if (osNameLowerCase.indexOf("linux") > -1) 
        {
            if (bHasModel)
            {
                System.out.println("Found sdbj.LX.");
            }
            else
            {
                System.out.println("!!! Could not find sdbj.LX !!!");
            }
        }
        if ( bHasJClasses )
        {
            System.out.println("Found JClass components.");
        }
        else
        {
            System.out.println("!!! Could not find JClass components !!!");
        }
        
        if ( bHasFLDAT )
        {
            System.out.println("Found FLDAT components.");
        }
        else
        {
            System.out.println("!!! Could not find FLDAT components !!!");
        }
        
        // If the user has a database, display the DAMCAT search tool
        if ( ( bHasDatabase ) && ( args.length == 0 ) ) 
        {
            Search searchTool = new Search(bHasModel);
            // If the option to display all dams is set
            if ( bHasListAllDams ) 
            {			
                searchTool.handleListAllDams();
            }
        }
        // If the user has a database, and NIDID is passed from the command line
        else if ( ( bHasDatabase ) && ( args.length != 0 ) )
        {
            String nidid = args[0];
            launcher.connOutMan(nidid);	
        }
        // If the user does not have a database, display the SMPDBK model input editor
        else 
        {
            boolean hasDatabase = false;
            String strWorkingDamNIDID = null;
            AnalysisData currentData = null;
            DBAccess dbAccess = null;
            
            ModelGUI modelInputEditor = new ModelGUI( hasDatabase, strWorkingDamNIDID, currentData, dbAccess );
        }
        
    }
    /**
     * Set value of private method from within Launcher.
     * Creation date: (10/3/2003 5:20:08 PM)
     * @param newBDBMSActive boolean
     */
    private void setDBMSActive( boolean newBDBMSActive ) 
    {
        bDBMSActive = newBDBMSActive;
    }
}
