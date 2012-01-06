package ohd.hseb.dimensions_file_uploader;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;

public class Dimensions_File_Uploader 
{
	private String _attributesDir = "";
    private String _fileToBeUploadedString = "";
    private String _drNumberString = "";
    private String _buildString = "";
    private String _username = null;
    private String _password = null;
    private static final String OB8_1_WORKSET = "AWIPS:OHD-OB8.1";
    private static final String OB8_2_WORKSET = "AWIPS:OHD-OB8.2";
    private static final String OB8_3_WORKSET = "AWIPS:OHD-OB8.3";
    private static final String OB8_1_1_WORKSET = "AWIPS:OHD-OB8.1.1";
    private String _workset = null;
	
	public Dimensions_File_Uploader( String attributesDir, String[] args )
	{
		_attributesDir = attributesDir;
		_drNumberString = args[ 0 ];
        
	}

	private void processAttributesFileDir()
	{
		File attributesDirFile = new File( _attributesDir );
		File[] attributesFileArray = attributesDirFile.listFiles();
        getUserNamePassword();

		for ( int i = 0; i < attributesFileArray.length; i++ )
		{
			File attributeFile = attributesFileArray[ i ];
			
			processAttributeFile( attributeFile );
		}
		attributesDirFile.delete();
	}

	private void processAttributeFile( File attributeFile )
	{
        FileReader fileReader = null;
        BufferedReader bufferedReader = null;
        String line = null;
        String relativeFilePath = null;
        String DRDCSNumberString = null;
        String fileStatusString = null;
        String commentsString = null;
        String PCMScommandString = null;
        
		if ( ! attributeFile.isDirectory() )       // Checks if the local file is a directory
		{
			try
			{
				fileReader = new FileReader( attributeFile );
				bufferedReader = new BufferedReader( fileReader );
				
				line = bufferedReader.readLine(); //read the relateive filepath of the source file
				relativeFilePath = line.substring( 1 );
				_fileToBeUploadedString = relativeFilePath;
				
				line = bufferedReader.readLine(); //read the DR/DCS #
				DRDCSNumberString = line; 
				
				line = bufferedReader.readLine(); //read the status of the file.  
				fileStatusString = line;
				
                line = bufferedReader.readLine(); //read the build level
                _buildString = line;
                setWorkset();
                
				line = bufferedReader.readLine(); //read the comments for the file
				commentsString = line;
			}
			
			catch (FileNotFoundException e)
			{
				
			}
			catch (IOException e)
			{
				
			}

			if ( fileStatusString.equalsIgnoreCase( "OLD" ) )
			{
				PCMScommandString = GetUpdateItemString(relativeFilePath, DRDCSNumberString, commentsString );
			}
			else
			{
				PCMScommandString = GetCreateItemString(relativeFilePath, DRDCSNumberString, commentsString );
			}
			
			RunPCMSCommandLineTool( PCMScommandString, attributeFile, DRDCSNumberString );
		}
	}
	
    private void setWorkset()
    {
        if ( _buildString.equalsIgnoreCase( "ob81" ) )
        {
            _workset = OB8_1_WORKSET;
        }
        else if ( _buildString.equalsIgnoreCase( "ob811" ) )
        {
            _workset = OB8_1_1_WORKSET;
        }
        else if ( _buildString.equalsIgnoreCase( "ob82" ) )
        {
            _workset = OB8_2_WORKSET;
        }
        else if ( _buildString.equalsIgnoreCase( "ob83" ) )
        {
            _workset = OB8_3_WORKSET;
        }
    }
	private void RunPCMSCommandLineTool( String PCMSCommandString, File attributeFile, String DRDCSNumberString )
	{
        String drDCSString = DRDCSNumberString.replaceAll( "AWIPS_", "" );
        String commandString = "/awips/hydroapps/PVCSFileQueue/upload_file_to_dimensions " + _fileToBeUploadedString + 
                               " " + _username + " " + _password + " " + drDCSString + " " + PCMSCommandString;
		String successfulFilePath = "/awips/hydroapps/PVCSFileQueue/filelist.success/" + _drNumberString;
		String failureFilePath = "/awips/hydroapps/PVCSFileQueue/filelist.failed/" + _drNumberString;
		File successfulFilePathFile = new File( successfulFilePath );
		File failureFilePathFile = new File( failureFilePath );
		
        System.out.println( "commandString: " + commandString );
        System.out.println( "PCMSCommandString: " + PCMSCommandString );
		InputStream inputStream = null;
		
		try 
		{
			Process process = Runtime.getRuntime().exec(commandString);
			inputStream = new BufferedInputStream( process.getInputStream() );
			process.waitFor();
			int ic = inputStream.read();
			while (ic  != -1 )
			{
				char c = (char ) ic;
				System.out.print( c );
				ic = inputStream.read();
			}


			if ( process.exitValue() == 0 )
			{
				if ( ! successfulFilePathFile.exists() )
				{
					successfulFilePathFile.mkdir();
				}
				attributeFile.renameTo( new File( successfulFilePath + "/" + attributeFile.getName() ) );
			}
			else 
			{
				if ( ! failureFilePathFile.exists() )
				{
					failureFilePathFile.mkdir();
				}
				attributeFile.renameTo( new File( failureFilePath + "/" + attributeFile.getName() ) );
			}
				
		}
		
		catch (InterruptedException e) 
		{
			e.printStackTrace();
		} 
		catch (IOException e) {
			e.printStackTrace();
		}
	}
	
    private void getUserNamePassword()
    {
        PasswordDialog passwordDialog = new PasswordDialog();
        passwordDialog.requestPasswordByDialog( "Please enter your Dimensions username/password" );
        
        _username = passwordDialog.getUserName();
        _password = passwordDialog.getPassword();
    }

    private String GetCreateItemString( String relativeString, String DRDCSNumberString, String commentsString )
	{
		String CIString = null;
		
//		CI "AWIPS:.AAAA-DEVSRC;1" /PART="AWIPS:SOFTWARE.AAAA;1" /FILENAME="fs/hseb/ob81/ohd/whfs/TEST_EMPTY_FILE.txt" /KEEP 
//		/FORMAT="DEVSRC" /WORKSET="AWIPS:OHD-OB8.1" /CHANGE_DOC_IDS="AWIPS_DR_18222"
		
		CIString = "CI \"AWIPS:.AAAA-DEVSRC;1\" /PART=\"AWIPS:SOFTWARE.AAAA;1\" /FILENAME=\"" + relativeString +
		           "\" /STATUS=\"REVIEW\" /USER_FILENAME=\"" + relativeString + "\" /KEEP /COMMENT=\"" + commentsString + "\" /FORMAT=\"DEVSRC\" /WORKSET=\"" + _workset + "\" " + 
		           " /CHANGE_DOC_IDS=\"" + DRDCSNumberString + "\"";
		
		return CIString;
	}
	
	private String GetUpdateItemString( String relativeString, String DRDCSNumberString, String commentsString )
	{
		String UIString = null;
		
//		UI AWIPS:; /FILENAME="fs/hseb/bld/devl_sys/cmd/update_ticket_no" /KEEP /CHANGE_DOC_IDS="AWIPS_DR_18222" 
//		/COMMENT="TESTING checkin script" /WORKSET="AWIPS:OHD-OB8.1"
		
		UIString = "UI AWIPS:; /FILENAME=\"" + relativeString + "\" /USER_FILENAME=\"" + relativeString + "\" /CHANGE_DOC_IDS=\"" + DRDCSNumberString +
		           "\" /STATUS=\"REVIEW\" /KEEP /COMMENT=\"" + commentsString + "\" /WORKSET=\"" + _workset + "\"";
		
		return UIString;
	}
	
	public static String validateDir( String[] args )
	{
		String inputDirString;
		File inputDirStringFile = null;
		
		inputDirString = "/awips/hydroapps/PVCSFileQueue/filelist/" + args[ 0 ];
		
		inputDirStringFile = new File( inputDirString);
		
		if ( ! inputDirStringFile.isDirectory() )
		{
			System.out.println( "Invalid DR #" );
			System.exit( 0 );
		}

		return inputDirString;
	}
	
	public static void main(String[] args) 
	{
		String uploadDirString = Dimensions_File_Uploader.validateDir( args );
		Dimensions_File_Uploader dimensions_file_uploader = new Dimensions_File_Uploader( uploadDirString, args );
		dimensions_file_uploader.processAttributesFileDir();
	}

}
