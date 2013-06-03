/*
 * Created on Oct 31, 2003
 *
 */
package ohd.hseb.util;

/**
 * @author Chip Gobs
 */
public class EndianConverter
{
    
    public static int convert(int origEndianInt)
    {
    
        int newEndianInt = 
                    (( origEndianInt & 0x000000ff )<< 24) |
                    (( origEndianInt & 0x0000ff00) << 8)  |
                    (( origEndianInt & 0x00ff0000) >>> 8)   |
                    (( origEndianInt & 0xff000000) >>> 24);
        /*  
          System.out.println("MAP.convertEndian(int): original int was " + origEndianInt + 
                              " new one is newEndianInt " + 
                              newEndianInt);
        */
      
        return newEndianInt;
        
    }
//      ----------------------------------------------------
  
     public static short convert(short origEndianShort)
     {
          short newEndianShort = 
                (short)( (( origEndianShort & 0x000000ff )<< 8) |
                      (( origEndianShort & 0x0000ff00) >>> 8)
                    );
                
                          
          return newEndianShort;
        
     } //end convertEndian for shorts

//     ----------------------------------------------------------
     
     public static float convert(float origEndianFloat)
     {
     
         int intBits = Float.floatToIntBits(origEndianFloat);
         
              
         int flippedInt = 
                     (( intBits & 0x000000ff )<< 24) |
                     (( intBits & 0x0000ff00) << 8)  |
                     (( intBits & 0x00ff0000) >>> 8)   |
                     (( intBits & 0xff000000) >>> 24);
        
         float newEndianFloat = Float.intBitsToFloat(flippedInt);
             
       
         return newEndianFloat;
         
     }
//       ----------------------------------------------------

}
