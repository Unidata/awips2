/*
 * Created on Jul 10, 2003
 * 
 * Modified 12/28/04 to provide getInt() method.
 */
package ohd.hseb.util;

import java.util.*;
import java.io.*;

/**
 * @author Chip Gobs
 *
 * This is the Java version of get_apps_defaults.c
 */
public class AppsDefaults
{
 
	
	private static final String APPS_DEFAULTS_USER = "APPS_DEFAULTS_USER";
	private static final String APPS_DEFAULTS_PROG = "APPS_DEFAULTS_PROG";
	private static final String APPS_DEFAULTS_SITE = "APPS_DEFAULTS_SITE";
	private static final String APPS_DEFAULTS = "APPS_DEFAULTS";
	
	private static final String RFR_OPEN= "$(";
	private static final String RFR_CLOSE = ")";
	
	private static final char DELIM = ':';
	private static final char COMMENT = '#';
	
	private static final char DOUBLE_QUOTE = '\"';
	private static final char SINGLE_QUOTE = '\'';
		
	private static final int RECUR_LIMIT = 40;
    
    private static Set _trueSet = new HashSet();
    
    private int _recursionCount = 0;	
		
	private Properties _envProperties = new Properties();
	
	private String _appsDefaultsUserFilePath = null;
	private String _appsDefaultsProgramFilePath = null;
	private String _appsDefaultsSiteFilePath = null;
	private String _appsDefaultsNationalFilePath = null;
	
	private BufferedReader _reader  = null;
	
    static
    {
        _trueSet.add("true");
        _trueSet.add("on");
        _trueSet.add("yes");
        _trueSet.add("y");   
        _trueSet.add("1");   
    }
    
//--------------------------------------------------------------
    public AppsDefaults()
    {
		EnvHelper envHelper = new EnvHelper();
	   
		_envProperties = envHelper.getProperties();
		
		_appsDefaultsUserFilePath = _envProperties.getProperty(APPS_DEFAULTS_USER);
		_appsDefaultsProgramFilePath = _envProperties.getProperty(APPS_DEFAULTS_PROG);
		_appsDefaultsSiteFilePath = _envProperties.getProperty(APPS_DEFAULTS_SITE);
		_appsDefaultsNationalFilePath = _envProperties.getProperty(APPS_DEFAULTS);
	 
	    return;
    }
//	-----------------------------------------------------	
    public int getInt(String tokenName, int defaultValue)
    {
        int returnValue = defaultValue;
  
        try
        {
            String tokenValue = getToken(tokenName);
            if (tokenValue != null)
            {
                returnValue = Integer.parseInt(tokenValue);
            }
        }
        catch (Throwable e)
        {
            returnValue = defaultValue;
        }
        
        
        return returnValue;
    }     
//	----------------------------------------------------
    public boolean getBoolean(String tokenName, boolean defaultValue, String trueValueString)
    {
        _trueSet.add(trueValueString);
        return getBoolean(tokenName, defaultValue);     
    }
//  -----------------------------------------------------   
    
    
    public boolean getBoolean(String tokenName, boolean defaultValue)
    {
        boolean returnValue = defaultValue;
               
        try
        {
            String tokenValue = getToken(tokenName);
            if (tokenValue != null)
            {
                if (_trueSet.contains(tokenValue.toLowerCase()))
                {
                    returnValue = true;
                }
                else
                {
                    returnValue = false;
                }
                       
            }
        }
        catch (Throwable e)
        {
            returnValue = defaultValue;
        }
        
        
        return returnValue;
    }    
    
//  -----------------------------------------------------   
    public String getToken(String tokenName, String defaultValue)
    {
    
        String tokenValue = getToken(tokenName);
        if (tokenValue == null)
        {
            tokenValue = defaultValue;
        }
      
        return tokenValue;
    }
//  -----------------------------------------------------   
   
    
	public String getToken(String tokenName)
	{
	    String tokenValue = null;
	    
	    //System.out.println("recursion count = " + _recursionCount);
	   
	    String envValue = _envProperties.getProperty(tokenName);
	     
	    // if token is available as an environment variable, use its value
	    if (envValue != null)
	    {
	    	tokenValue = envValue;
	    }     	
	    else // look for the token in each the files (if they are defined), until 
	        // find the token, then stop looking as soon as it is found
	    {
	        tokenValue = getTokenFromFile(tokenName, _appsDefaultsUserFilePath);
	    	if (tokenValue == null)
	    	{
	    	    tokenValue = getTokenFromFile(tokenName, _appsDefaultsProgramFilePath);
				if (tokenValue == null)
				{
				    tokenValue = getTokenFromFile(tokenName, _appsDefaultsSiteFilePath);
					if (tokenValue == null)
					{
					    tokenValue = getTokenFromFile(tokenName, _appsDefaultsNationalFilePath);
					}
				}
	    	}
	    } //end else
	      
	    tokenValue = expandReferBacks(tokenValue);
	
	    return tokenValue;

	} //end getToken()
	
//	--------------------------------------------------------------

    private String getTokenFromFile(String tokenName, String filePath)
    {
        String tokenValue = null;
        boolean tokenFound = false;
        String line = "";
        
        NameValuePair nameValuePair = null;
     
		if (filePath != null)
		{
      
	        try
	        {
	            _reader = new BufferedReader (new FileReader(filePath));
	            
	            while ((! tokenFound) && (line != null) )
	            {
	                 line = getLine();
					 if (line == null)
					 {
                          break;
					 }
	                 nameValuePair = parseLine(line);          

	                 if (nameValuePair != null)
					 {
						// System.out.println("nameValuePair.getName() = |" +
						//					 nameValuePair.getName() + "|" );

						// System.out.println("nameValuePair.getValue() = |" +
						//					 nameValuePair.getValue() + "|" );
 
	                     if ( (nameValuePair.getName().equals(tokenName)) )
	                   
	                     {
	                         tokenFound = true;
	                         tokenValue = nameValuePair.getValue();
	                     }
					 }	
	            }
	        
	            _reader.close();
	        
	        }
	        catch (java.io.IOException e)
	        {
	        	
	        }
        
		}
        return tokenValue;	
    }

//	-----------------------------------------------------
	private String expandReferBacks(String tokenValue)
	{
		if (tokenValue != null)
		{
			while (thereAreReferBacks(tokenValue))
			{
				//System.out.println("tokenValue before expandFirstReferBack = " + tokenValue);
				tokenValue = expandFirstReferBack(tokenValue);
				//System.out.println("tokenValue after expandFirstReferBack = " + tokenValue);	
			}
		}	
		
		return tokenValue;
	}
  
//	-----------------------------------------------------
	    
	private boolean thereAreReferBacks(String tokenValue)
	{
		boolean result = false;
		
		if (tokenValue.indexOf(RFR_OPEN) > -1)
		{
			result = true;	
		}
		
		return result;		
	}

//	-----------------------------------------------------
		
   private String expandFirstReferBack(String tokenValue)
   {
	     
	   int referBackStartIndex = tokenValue.indexOf(RFR_OPEN);
	   int referBackEndIndex = tokenValue.indexOf(RFR_CLOSE);	   
	   String beginning = "";
	   String middle = null;
	   String newTokenName = null;
	   String end = "";
     
	   if ((referBackStartIndex > -1) && (referBackEndIndex > -1))
	   {
		   if (referBackStartIndex > 0)
		   {
			  beginning = tokenValue.substring(0, referBackStartIndex);
		   }
	   	   
		   newTokenName = tokenValue.substring(referBackStartIndex + RFR_OPEN.length(),
											 referBackEndIndex); 	
	                                         
		   _recursionCount++;
	       
		   if (_recursionCount <= RECUR_LIMIT)
		   {
			   middle = getToken(newTokenName);	
			   _recursionCount--;	  
		   }
		   else
		   {
			   middle = "ERROR_ERROR_ERROR";
			   System.err.println("You probably have a cycle in your Apps Defaults File's  refer backs, please check it");
		   }
		   if ( (referBackEndIndex + RFR_CLOSE.length()) < tokenValue.length() )
		   {
				 end = tokenValue.substring(referBackEndIndex + RFR_CLOSE.length(),
										 tokenValue.length());
		   }
		  

		   tokenValue = beginning + middle + end;
	   }	
		
	   return tokenValue;
   }
		
//	-----------------------------------------------------	
	private String getLine() throws java.io.IOException
	{
	    String line = null;
	    
	    line = _reader.readLine();
	    
	   // System.out.println("line = " + line);
	    
	    return line;	
	}
//	-----------------------------------------------------
	private NameValuePair parseLine(String line)
	{
	    NameValuePair pair = null;
		int delimiterIndex = -1;
		
		String tokenName = null;
		String tokenValue = null;
	   	
	  	//find delimiter
	    	 
	   	delimiterIndex = line.indexOf(DELIM);
	   	
	
	   	if (delimiterIndex > -1) //there is a delimiter character on the line
	   	{
			//find tokenName
	        tokenName = findTokenName(line, delimiterIndex);
	         
	        if (tokenName != null)
	        {
	            tokenValue = findTokenValue(line, delimiterIndex);	
	            if (tokenValue != null)
	            {
	                pair = new NameValuePair(tokenName, tokenValue);
	            }
	        } 
	   	} // end if found a delimiter
	   	else //there is no delimiter, so can't read a pair from this line
	   	{
	   	 	pair = null;
	   	}
	
	    return pair;	
	}
	
//	-----------------------------------------------------
	private String findTokenName(String line, int delimiterIndex)
	{
		String tokenName = null;
		boolean foundTokenName = false;
		boolean foundStartOfTokenName = false;
		boolean foundComment = false;
		StringBuffer tokenNameBuffer = new StringBuffer(); 

		
		for (int i = 0; ((i < delimiterIndex) &&
									  (! foundTokenName)) &&
									  (! foundComment) ; i++)
		{
			char c = line.charAt(i);
			if (isWhiteSpace(c))
			{
				// check to see if this is white space at the beginning or the end
				// of the tokenName
				if (! foundStartOfTokenName) //this must beginning whitespace
				{
						// so ignore the whitespace
				}
				else //must be trailing whitespace
				{
					// the token is done;
					tokenName = tokenNameBuffer.toString();
					foundTokenName = true;
				}
			} //end if isWhiteSpace
    
			else if (isCommentStarter(c))
			{
				//There can't be a valid tokenName, tokenValue pair here, then
				foundComment = true;
    	
				//clear out the tokenNameVariables
				tokenName = null;
				
				// works in  >= java 1.2, 
				//tokenNameBuffer.delete(0, tokenNameBuffer.length());
				
				//works in java < 1.2, but the previous line is prefered
				tokenNameBuffer = new StringBuffer();
				
				foundStartOfTokenName = false;
				break; //exit loop
    	
			} //end isCommentStarter
			
			else //part of the tokenName
			{
				tokenNameBuffer.append(c);
				foundStartOfTokenName = true;
			}
    
		} //end for

		if (foundStartOfTokenName)
		{
			tokenName = tokenNameBuffer.toString();	
		}

        return tokenName;
	}
	
	//----------------------------------------------------------------------
	
	private String findTokenValue(String line, int delimiterIndex)
	{
			String tokenValue = null;
			
			boolean foundTokenValue = false;
			boolean foundStartOfTokenValue = false;
			boolean foundComment = false;
			
			boolean foundSingleOpenQuote = false;
			boolean foundSingleCloseQuote = false;
	
			boolean foundDoubleOpenQuote = false;
			boolean foundDoubleCloseQuote = false;

			boolean error = false;
			
			StringBuffer tokenValueBuffer = new StringBuffer(); 

			for (int i = delimiterIndex + 1 ; ((i < line.length()) &&
										  (! foundTokenValue)) &&
										  (! foundComment) ; i++)
			{
				char c = line.charAt(i);
				if (isWhiteSpace(c))
				{
					// check to see if this is white space at the beginning or the end
					// of the tokenValue
					if (! foundStartOfTokenValue) //this must be beginning whitespace
					{
							// so ignore the whitespace
					}
					else if ((foundSingleOpenQuote) && (!foundSingleCloseQuote))
					{
						tokenValueBuffer.append(c);
						foundStartOfTokenValue = true;
					}
					else if ((foundDoubleOpenQuote) && (!foundDoubleCloseQuote))
					{
						tokenValueBuffer.append(c);
						foundStartOfTokenValue = true;
					}				
					else //must be trailing whitespace
					{
						// the token value reading is done;
						tokenValue = tokenValueBuffer.toString();
						foundTokenValue = true;
					}
				} //end if isWhiteSpace
    
				else if (isCommentStarter(c))
				{
					if (foundStartOfTokenValue )
					{
						//this character is allowed in a tokenValue
						tokenValueBuffer.append(c);
					}
					else 
					{  //error, there can't be a valid tokenValue
						foundComment = true;
	    	
						//clear out the tokenNameVariables
						tokenValue = null;
				
						//	works in  >= java 1.2, 
						//tokenValueBuffer.delete(0, tokenValueBuffer.length());
				
					    //works in java < 1.2, but the previous line is prefered
					    tokenValueBuffer = new StringBuffer();
						
						
						error = true;
					}
				} //end isCommentStarter
				
				else if (isDelimiter(c))
				{
					if (foundStartOfTokenValue )
					{
						//this character is allowed in a tokenValue
						tokenValueBuffer.append(c);
					}
					else 
					{  //error, there can't be a valid tokenValue
				
						//clear out the tokenNameVariables
						tokenValue = null;
						
						//	works in  >= java 1.2, 
						//tokenValueBuffer.delete(0, tokenValueBuffer.length());
				
					    //works in java < 1.2, but the previous line is prefered
						tokenValueBuffer = new StringBuffer();
						
						error = true;
						break; //exit loop
					}
					
				}
				else if (isSingleQuote(c))
				{
					if (foundSingleOpenQuote)
					{
					    foundSingleCloseQuote = true;
					    foundTokenValue = true; //done
					}
					else
					{
					    foundSingleOpenQuote = true;
					}
				}
				
				else if (isDoubleQuote(c))
				{
					if (foundDoubleOpenQuote)
					{
						foundDoubleCloseQuote = true;
						foundTokenValue = true; //done
					}
					else
					{	
					    foundDoubleOpenQuote = true;
					}
				}
				
				else //part of the tokenValue
				{
					tokenValueBuffer.append(c);
                 //   System.out.println("tokenValueBuffer =" + tokenValueBuffer.toString());
					foundStartOfTokenValue = true;
				}
    
			} //end for

			if ( (foundStartOfTokenValue) && (!error))
			{
				tokenValue = tokenValueBuffer.toString();	
			}

			return tokenValue;
		}
	// -----------------------------------------------------
	

//	-----------------------------------------------------

	
	//
	private static boolean isWhiteSpace(char c)
	{
	    boolean result = false;
    	 
		if ( (c == ' ') || (c== '\t') )
		{
			result = true;		
		}	 
		
		return result;
		
	} //isWhiteSpace
    
//	-----------------------------------------------------
	private static boolean isCommentStarter(char c)
	{
	    boolean result = false;
    	 
		if (c == COMMENT)
		{
			result = true;		
		}	 
		
		return result;	
	} //isCommentStarter
    
    //-------------------------------------------------------------
	private static boolean isDelimiter(char c)
		{
			boolean result = false;
    	 
			if (c == DELIM)
			{
				result = true;	
			
			}	 
		
			return result;	
		} //isDelimiter
    
    //-----------------------------------------------------------
	
	private static boolean isSingleQuote(char c)
	{
		boolean result = false;
    	 
		if (c == SINGLE_QUOTE)
		{
			result = true;	
		}	 
		
		return result;	
	} //isSingleQuote
    
//		-----------------------------------------------------
	
	private static boolean isDoubleQuote(char c)
	{
	    boolean result = false;
    	 
		if (c == DOUBLE_QUOTE)
		{
			result = true;	
		}	 
		
		return result;	
	
	} //isDoubleQuote
    
    
//			-----------------------------------------------------
    public static void main(String[] args)
    {
        AppsDefaults ad = new AppsDefaults();
        String tokenName = null;
		String tokenValue = null;

       // tokenName = "shef_procobs";
       //  tokenValue = ad.getToken(tokenName);
        
       // System.out.println("tokenName = " + tokenName +
       //                     " tokenValue = " + tokenValue);	
                      
        tokenName = "ffg_out_dir";
        tokenValue = ad.getToken(tokenName);
        
        System.out.println("tokenName = " + tokenName +
                           " tokenValue = " + tokenValue);	
                           
                           
        tokenName = "testToken1";
        //tokenRawValue = "#hank:me:'llhank' "
        tokenValue = ad.getToken(tokenName);  
		System.out.println("tokenName = " + tokenName +
								  " tokenValue = " + tokenValue);	
								  
		tokenName = "testToken2";
		//tokenRawValue = "dd#hank:me:'llhank' "
		tokenValue = ad.getToken(tokenName);  
		System.out.println("tokenName = " + tokenName +
										" tokenValue = " + tokenValue);	
		

		tokenName = "testToken3";
		//tokenRawValue = 'dd#hank:me:"llhank" '
		tokenValue = ad.getToken(tokenName);  
		System.out.println("tokenName = " + tokenName +
							" tokenValue = " + tokenValue);									                    
                           
    }


} // end class AppsDefaults
