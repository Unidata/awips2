package ohd.hseb.util;


import java.util.*;

public class StringParser
{
	public StringParser()
	{
	}

	public static String[] tokenize( String stringToTokenize )
	{
		StringTokenizer tokens = new StringTokenizer( stringToTokenize );
		List tokenList = new ArrayList();
		
		while ( tokens.hasMoreTokens() )
		{
			tokenList.add( tokens.nextToken() );
		}
		
		String[] stringArray = new String[ tokenList.size() ];
		
		for ( int i = 0; i < tokenList.size(); i++ )
		{
			stringArray[ i ] = (String) tokenList.get( i );
		}
		
		return stringArray;
	}
	
	public String toString( List tokenList )
	{
		StringBuffer output = new StringBuffer();
		String token = null;
		
		for ( int i = 0; i < tokenList.size(); i++ )
		{
			token = (String) tokenList.get( i );
			output.append( token + "      " );
		}
		return output.toString();
	}
}
