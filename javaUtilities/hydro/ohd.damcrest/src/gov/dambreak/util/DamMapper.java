package gov.dambreak.util;

// import com.bbn.openmap.layer.shape.*;
// import com.bbn.openmap.*;
// import com.bbn.openmap.layer.*;
// import com.bbn.openmap.layer.location.*;
import javax.swing.*;
/**
 * Class to encapsulate interface to OpenMap software package
 * Creation date: (8/5/2003 9:29:21 AM)
 * @author: 
 */
public class DamMapper extends JFrame {
    /**
     * DamMapper constructor comment.
     * @param title java.lang.String
     */
    public DamMapper(String title) {
        super(title); 
        initialize();
        setSize(400,400);
        setLocation(616,150);
        setVisible(true);
    }
    /**
     * Initialize for OpenMap interface 
     * Creation date: (8/5/2003 9:29:43 AM)
     */
    private void initialize() {
        
        // Create a MapBean
        
        /*
         MapBean mapBean = new MapBean();
         */
        
        // Create a ShapeLayer to show world political boundaries.
        // Set the properties of the layer.  This assumes that the
        // datafiles "dcwpo-browse.shp" and "dcwpo-browse.ssx" are in
        // a path specified in the CLASSPATH variable.  These files
        // are distributed with OpenMap and reside in the toplevel
        // "share" subdirectory.
        
        /*
         ShapeLayer shapeLayer = new ShapeLayer();
         Properties shapeLayerProps = new Properties();
         shapeLayerProps.put("prettyName", "Political Solid");
         shapeLayerProps.put("lineColor", "000000");
         shapeLayerProps.put("fillColor", "BDDE83");
         shapeLayerProps.put("shapeFile", "D:/openMap/openmap-4.5.4/share/data/shape/dcwpo-browse.shp");
         shapeLayerProps.put("spatialIndex", "D:/openMap/openmap-4.5.4/share/data/shape/dcwpo-browse.ssx");
         shapeLayer.setProperties(shapeLayerProps);
         */
        
        // Add the political layer to the map
        /*
         mapBean.add(shapeLayer);
         
         LocationLayer loc = new LocationLayer();
         
         
         loc.setLocationHandlers( new LocationHandler[] { new BasicLocationHandler() } );
         
         mapBean.add(loc);
         */
        
        // Add the map to the frame
        /*
         getContentPane().add(mapBean);
         */
    }
}
