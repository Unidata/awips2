/*
 * Created on Aug 28, 2003
 *
 * 
 */
package ohd.hseb.util;

import java.util.*;



public class Cryptor
{
	
   private String _key = "adlfkjaldfkjaldkjf";
   private static long _seed = 5234234234L; 
   private static int _range = 17;
   
   // ---------------------------------------------------------- 
   public static String decrypt(String encryptedString)
   {
       StringBuffer buffer = new StringBuffer(encryptedString);
	
	   Random random = new Random(_seed); 
       for (int i = 0; i < buffer.length(); i++)
       {
           char c = buffer.charAt(i);
           c -= getNextShift(random); 
		   buffer.setCharAt(i,c);		 
       }
       
	  // System.out.println("");
       
   	   return buffer.toString();	
   }
   
   // ---------------------------------------------------------- 
   
    public static String encrypt(String encryptedString)
    {
        StringBuffer buffer = new StringBuffer(encryptedString);
	
        Random random = new Random(_seed); 
        for (int i = 0; i < buffer.length(); i++)
        {
	        char c = buffer.charAt(i);
	        c += getNextShift(random); 	 
	        buffer.setCharAt(i,c);
        }
       
       // System.out.println("");
       
        return buffer.toString();	
   }
   
   // ---------------------------------------------------
   private static int getNextShift(Random random)
   {
   	   int shift = random.nextInt(2 * _range) - _range;
   	  // System.out.print("shift = " + shift + " "); 
       return shift;
   }
   
   // ---------------------------------------------------------- 
   
   public static void main(String[] args)
   {
       String message = "";
       String encMessage = "";
       
       String  decMessage = "";
       
       encMessage = encrypt(message);
       decMessage = decrypt(encMessage);
   	
   	
   	  // System.out.println("message = " + message);
	   System.out.println("encMessage = " + encMessage);
	   //System.out.println("decMessage = " + decMessage);
   	   
   }

}
