/*
 * Created on Oct 8, 2003
 *
 * 
 */
package ohd.hseb.db;


/**
 * @author Chip Gobs
 *
 * 
 */
public class DbConnectionHelper
{
	String _baseConnectionString = null;
	String _propertiesFilePath = null;

//	------------------------------------------------------------------------	

	public DbConnectionHelper(String baseConnectionString)
	{
		_baseConnectionString = baseConnectionString;	
	}
	
//	------------------------------------------------------------------------	

	public DbConnectionHelper(String baseConnectionString, String propertiesFilePath)
	{
			_baseConnectionString = baseConnectionString;	
			_propertiesFilePath = propertiesFilePath;
	}
//	------------------------------------------------------------------------	
	public String getConnectionString()
	{
		String connectionString = _baseConnectionString;
		
	
		return connectionString;
	} //end getConnectionString
	
	// ------------------------------------------------------------------------
/*	private String getConnectionStringByGui()
	{
		String connectionStringBase = _baseConnectionString;
				   
		String connectionString = null;
	
	
		PasswordDialog dialog = new PasswordDialog();
	
		String message = "Enter your user name and password for the HP";
		boolean success = dialog.requestPasswordByDialog(message);
	
		String userName = dialog.getUserName();
		String password = dialog.getPassword();
			
		connectionString = connectionStringBase + 
                           "user="+ userName +
                            ";password=" + password +";";		
		
		
			return connectionString;
	}  //end getConnectionStringByGui
   
		// ------------------------------------------------------------------------

	private String getDecryptedConnectionString()
	{
		//
		//String url ="jdbc:informix-sqli:" + "//ds1:" +
		//				"1530/hd_ob3ounx:" + "informixserver=ONLINE";

		String connectionURLPart = _baseConnectionString;
		String connectionString = null;
	
		String encryptedUserName = null;
		String encryptedPassword = null;

		String userName = null;
		String password = null;

		Properties props = new Properties();
		try
		{
			props.load(new FileInputStream(_propertiesFilePath));
			encryptedUserName = props.getProperty("un");
			encryptedPassword = props.getProperty("pw");
		
			userName = Cryptor.decrypt(encryptedUserName);
			password = Cryptor.decrypt(encryptedPassword);

			connectionString = connectionURLPart + 
                               "user="+ userName +
                               ";password=" + password +";";

		}
		catch (FileNotFoundException e)
		{
			//System.err.println("You need a properties file named Data.props");
			//e.printStackTrace();		
		}
		catch(IOException e)
		{
			//e.printStackTrace();
		}
	
		return connectionString;
	} //end getDecryptedConnectionString()
*/	
	
}
