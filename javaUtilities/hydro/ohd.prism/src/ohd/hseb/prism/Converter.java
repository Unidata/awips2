package ohd.hseb.prism;

// import ohd.hseb.prism.*;

public class Converter implements MPEConstants
{
   static public class HrapPoint
   {
      private double hrap_row;
      private double hrap_col;

      public double getHrapRow ( )
      {
         return hrap_row;
      }

      public double getHrapCol ( )
      {
         return hrap_col;
      }
   }

   static public HrapPoint convertLatLonToHrap ( double longitude,
                                                 double latitude )
   {
      double r;
      HrapPoint hrap_point = new HrapPoint ( );

      latitude *= D2RAD;
      longitude = ( longitude + 180.0 - REF_LON ) * D2RAD;
      r = RE * Math.cos (latitude)/(1.0 + Math.sin(latitude));
      hrap_point.hrap_col = r * Math.sin(longitude);
      hrap_point.hrap_row = r * Math.cos(longitude);
      hrap_point.hrap_col += HRAP_BASE_COL;
      hrap_point.hrap_row += HRAP_BASE_ROW;

      return hrap_point;
   }

   static public void main ( String args [ ] )
   {
      HrapPoint hrap = convertLatLonToHrap ( 100.63, 33.48 );

      System.out.println ( "Row: " + hrap.hrap_row );
      System.out.println ( "Col: " + hrap.hrap_col );
   }
}
