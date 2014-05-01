package ohd.hseb.geomap.examples;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.KeyStroke;


import ohd.hseb.geomap.MapCanvas;
import ohd.hseb.geomap.MapDataManager;
import ohd.hseb.grid.XmrgGrid;
import ohd.hseb.sshp.precip.XmrgReader;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.Logger;
import ohd.hseb.geomap.contour.RowColToLatLonTranslator;
import ohd.hseb.geomap.io.BCDFileReader;
import ohd.hseb.geomap.io.BinaryGeoDataFileReader;
import ohd.hseb.geomap.io.ElevationFileReader;
import ohd.hseb.geomap.layer.BaseColorDeterminer;
import ohd.hseb.geomap.layer.CenterPointMapLayer;
import ohd.hseb.geomap.layer.ColorDeterminer;
import ohd.hseb.geomap.layer.ElevationMapLayer;
import ohd.hseb.geomap.layer.GriddedMapLayer;
import ohd.hseb.geomap.layer.MapLayer;
import ohd.hseb.geomap.layer.PolyLineMapLayer;
import ohd.hseb.geomap.layer.PolygonMapLayer;
import ohd.hseb.geomap.layer.XmrgGridToGridAdapter;
import ohd.hseb.geomap.model.ElevationMap;
import ohd.hseb.geomap.model.HrapRowColToLatLonConverter;
import ohd.hseb.geomap.model.LatLonBounds;
import ohd.hseb.geomap.model.LatLonPoint;
import ohd.hseb.geomap.model.LatLonPolyline;
import ohd.hseb.geomap.model.Polygon;
import ohd.hseb.geomap.projection.FlatMapProjection;
import ohd.hseb.geomap.projection.MapProjection;

public class MapFrame extends JFrame
{
    // ------------------------------------------------------------------------------------------------
    
    private JMenuBar _menuBar = null;
    
    private JPopupMenu _popupMenu = null;
    private Point _pointPopUpInvoked = null;
    
    
    private MapCanvas _mapCanvas = null;
    private MapProjection _mapProjection = null; 
    private MapDataManager _dataManager = null;
    
    // shared Data
    private ElevationMap _elevationMap = null;
    
    // station data
    List<StationPoint> _sshpFcstPointList = null;
    
    // station data
    List<StationPoint> _riverProFcstPointList = null;
    
    // station data
    List<StationPoint> _precipMonitorPointList = null;
    
    // layers
    
    private MapLayer _stateMapLayer = null;
    private MapLayer _countyMapLayer = null;
    private MapLayer _basinMapLayer = null;
    
    
    private MapLayer _riverMapLayer = null;
    private MapLayer _streamMapLayer = null;
    
    private MapLayer _elevationImageMapLayer = null;
    private MapLayer _elevationContourMapLayer = null;
    
    private MapLayer _qpeImageMapLayer = null;
    private MapLayer _qpeContourMapLayer = null;
 
    
    private MapLayer _centerPointMapLayer = null;
    private MapLayer _riverProPointMapLayer = null;
    private MapLayer _sshpPointMapLayer = null;
    private MapLayer _precipMonitorPointMapLayer = null;

    String _geoDir = "/awips/hydroapps/ob83_fwr/whfs/local/data/geo/";
    
    // ------------------------------------------------------------------------------------------------
  
    public MapFrame(MapDataManager dataManager)
    {
        _dataManager = dataManager;
        
        setTitle("MapFrame Experimental Window");
        
        this.addWindowListener(new FrameCloseWindowListener() );
        
        LatLonPoint centerLatLonPoint = new LatLonPoint(31.14, -98.12);
        
        
        _mapProjection = new FlatMapProjection(centerLatLonPoint);
       
    //   _mapProjection = new PolarProjection(centerLatLonPoint, 800);
        
        initGui();     
        
    }
    
    // ------------------------------------------------------------------------------------------------
        
    private void initGui()
    {
        
        Dimension mapSize = new Dimension(500, 500);  

        //init the map canvas
        _mapCanvas = new MapCanvas(_mapProjection, mapSize.width, mapSize.height);
        _mapCanvas.setBackground(Color.cyan);


        //create the mapLayers

        _elevationImageMapLayer = createElevationMapLayer();
        _elevationContourMapLayer = createElevationContourMapLayer(_elevationMap);

        _qpeImageMapLayer = createQpeMapLayer(true);
        _qpeContourMapLayer = createQpeMapLayer(false);


        _countyMapLayer = createCountyMapLayer();
        _countyMapLayer.setShouldDraw(false);

        _stateMapLayer = createStateMapLayer();
        _stateMapLayer.setShouldDraw(false);

        _centerPointMapLayer = new CenterPointMapLayer(10, Color.RED);

        
        _sshpFcstPointList = _dataManager.getSshpFcstPointList();
        _sshpPointMapLayer = new RiverFcstPointMapLayer(_sshpFcstPointList, Color.ORANGE);
        
        
        _precipMonitorPointList = _dataManager.getPrecipMonitorPointList();
        _precipMonitorPointMapLayer = new PrecipMonitorPointMapLayer(_precipMonitorPointList);
        
        _riverProFcstPointList = _dataManager.getRiverProFcstPointList();
        _riverProPointMapLayer = new RiverFcstPointMapLayer(_riverProFcstPointList, Color.green);
    
     
        //  String riverFilePath = "streams_latlon.LX";
         String riverFilePath = _geoDir + "/" + "rivers_latlon.LX";
         _riverMapLayer = createBinaryLineMapLayer(riverFilePath, Color.BLUE); 
        
         String streamFilePath = _geoDir + "/" + "streams_latlon.LX";
          _streamMapLayer = createBinaryLineMapLayer(streamFilePath, Color.cyan); 
         
        
         String basinFilePath = "basins_latlon.LX";
         _basinMapLayer = createBinaryLineMapLayer(_geoDir + "/" + basinFilePath, new Color(244, 164, 96)); 
         
         
        //add map layers
        _mapCanvas.addMapLayer(_elevationImageMapLayer); 
        _mapCanvas.addMapLayer(_elevationContourMapLayer); 
        _mapCanvas.addMapLayer(_stateMapLayer);
        _mapCanvas.addMapLayer(_countyMapLayer);
        _mapCanvas.addMapLayer(_basinMapLayer);
        _mapCanvas.addMapLayer(_riverMapLayer);
        _mapCanvas.addMapLayer(_streamMapLayer);
        
        _mapCanvas.addMapLayer(_qpeImageMapLayer);
        _mapCanvas.addMapLayer(_qpeContourMapLayer);
        
        _mapCanvas.addMapLayer(_sshpPointMapLayer);
        _mapCanvas.addMapLayer(_riverProPointMapLayer);
        _mapCanvas.addMapLayer(_precipMonitorPointMapLayer);
        _mapCanvas.addMapLayer(_centerPointMapLayer);
          
        _mapCanvas.setPreferredSize(mapSize);    
        //_mapCanvas.setBackground(Color.magenta);
           
         JPanel buttonPanel = createButtonPanel();
  
         Container contentPane =  this.getContentPane();
        contentPane.setBackground(Color.green);
          
  // setup the main panel
        
        JPanel panel = new JPanel();
        panel.setBackground(Color.yellow);

        GridBagLayout layoutMgr = new GridBagLayout();       
        GridBagConstraints mainGbc = new GridBagConstraints();
        
        mainGbc.fill = GridBagConstraints.BOTH;
        mainGbc.anchor = GridBagConstraints.NORTHWEST;
        mainGbc.weightx = 1;
        mainGbc.weighty = 1;
       
        panel.setLayout(layoutMgr);
                                                     //    col, row   numCols numRows  Wcol wrow   
        addComponent(panel, buttonPanel,         mainGbc,    0,   0,  1,      1,        0,   0, 0);
        addComponent(panel, _mapCanvas,          mainGbc,    0,   1,  1,      20,       1,   1, 1);
               
        _mapCanvas.setVisible(true);
        
      //  contentPane.setLayout(new BorderLayout());
        contentPane.add(panel);
         
        
        
        //init the menubar
        initMenuBar();
        setJMenuBar(_menuBar);
        
        // init the mapCanvas popupMenu
        initPopupMenu();
        
        //add popupListener
        _mapCanvas.addMouseListener(new PopupListener());
        
        //add other mouse button press listeners
        _mapCanvas.addMouseListener(new RecenterAndZoomListener());
        _mapCanvas.addMouseListener(new SelectStationListener());
        

        this.setSize(new Dimension(500, 500));
        this.pack();
        this.setVisible(true);
    }
    //  --------------------------------------------------------------------------------------------
    
    
    // --------------------------------------------------------------------------------------------
    
    private JPanel createButtonPanel()
    {
        JPanel buttonPanel = new JPanel();
        
        //create the buttons
        JButton zoomInButton = new JButton("Zoom In");
        JButton zoomOutButton = new JButton("Zoom Out");
        
        JButton westButton = new JButton("Pan West");
        JButton eastButton = new JButton("Pan East");
        JButton northButton = new JButton("Pan North");
        JButton southButton = new JButton("Pan South");
   
        //arrange them in the gui
        buttonPanel.add(zoomInButton);
        buttonPanel.add(zoomOutButton);
        
        //pan buttons
        buttonPanel.add(westButton);
        buttonPanel.add(eastButton);
        buttonPanel.add(northButton);
        buttonPanel.add(southButton);
          
        //add listeners
        zoomInButton.addActionListener(new ZoomListener("IN"));
        zoomOutButton.addActionListener(new ZoomListener("OUT"));
        
        northButton.addActionListener(new PanListener(PanListener.NORTH));
        southButton.addActionListener(new PanListener(PanListener.SOUTH));
        eastButton.addActionListener(new PanListener(PanListener.EAST));
        westButton.addActionListener(new PanListener(PanListener.WEST));
      
        return buttonPanel;
    }
    // ---------------------------------------------------------------------------------------------------------------
    
    private void initMenuBar()
    {
         _menuBar = new JMenuBar();
          
        JMenu menu = null;
        JMenuItem menuItem = null;
        JCheckBoxMenuItem checkBox = null;
  
        //File Menu
        menu = new JMenu("File");
        menu.setMnemonic(KeyEvent.VK_F);
        menu.getAccessibleContext().setAccessibleDescription(
                         "Access File Menus");
        _menuBar.add(menu);
        
        
        menuItem = new JMenuItem("Close Window");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_C, ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription(
                "Close this Window.");
        menuItem.addActionListener(new FrameCloseWindowListener());
        menu.add(menuItem); 
       

        // Edit/View Menu
        menu = new JMenu("View/Edit");
        menu.setMnemonic(KeyEvent.VK_V);
        menu.getAccessibleContext().setAccessibleDescription(
                         "View/Edit time series data.");
        _menuBar.add(menu);
        
          
        // edit menu items
        
        addMapLayerCheckBoxMenuItem(menu, "Show CenterPoint", _centerPointMapLayer);       
        addMapLayerCheckBoxMenuItem(menu, "Show States", _stateMapLayer);
        addMapLayerCheckBoxMenuItem(menu, "Show Counties", _countyMapLayer);
        addMapLayerCheckBoxMenuItem(menu, "Show Basins", _basinMapLayer);
        
        addMapLayerCheckBoxMenuItem(menu, "Show Rivers", _riverMapLayer);
        addMapLayerCheckBoxMenuItem(menu, "Show Streams", _streamMapLayer);
         
        addMapLayerCheckBoxMenuItem(menu, "Show QPE Image", _qpeImageMapLayer);
        addMapLayerCheckBoxMenuItem(menu, "Show QPE Contour", _qpeContourMapLayer);
       
        addMapLayerCheckBoxMenuItem(menu, "Show Elevation Image", _elevationImageMapLayer);
        addMapLayerCheckBoxMenuItem(menu, "Show Elevation Contour", _elevationContourMapLayer);
               
        addMapLayerCheckBoxMenuItem(menu, "Show SSHP Points", _sshpPointMapLayer);
        addMapLayerCheckBoxMenuItem(menu, "Show RiverPro Points", _riverProPointMapLayer);
        addMapLayerCheckBoxMenuItem(menu, "Show PrecipMonitor Points", _precipMonitorPointMapLayer);
      
    }
    //  --------------------------------------------------------------------------------------------
    private void addMapLayerCheckBoxMenuItem(JMenu menu, String text, MapLayer mapLayer)
    {
        JCheckBoxMenuItem checkBoxMenuItem =  new JCheckBoxMenuItem(text);
        menu.add( checkBoxMenuItem);
        
        checkBoxMenuItem.addActionListener(new MapLayerToggleButtonListener(mapLayer));
        mapLayer.setShouldDraw(false);
    }
    
    //  --------------------------------------------------------------------------------------------
    private void initPopupMenu()
    {
        _popupMenu = new JPopupMenu();
        JPopupMenu menu = _popupMenu;
        JMenuItem menuItem = null;
        
        
        // zooming
        
        menuItem = new JMenuItem("In");
        menuItem.addActionListener(new ZoomListener("IN"));      
        menu.add(menuItem); 
        
        menuItem = new JMenuItem("Out");
        menuItem.addActionListener(new ZoomListener("OUT"));
        menu.add(menuItem); 
              
        //recentering
        
        menuItem = new JMenuItem("Recenter");
        menuItem.addActionListener(new RecenterActionListener());
        menu.add(menuItem); 
        
        JSeparator separator = new JSeparator();
        menu.add(separator); 
        
        //panning
        menuItem = new JMenuItem("Up");
        menuItem.addActionListener(new PanListener(PanListener.NORTH));
        menu.add(menuItem); 
        
        menuItem = new JMenuItem("Down");
        menuItem.addActionListener(new PanListener(PanListener.SOUTH));
        menu.add(menuItem); 
        
        menuItem = new JMenuItem("Left");
        menuItem.addActionListener(new PanListener(PanListener.WEST));
        menu.add(menuItem); 
        
        menuItem = new JMenuItem("Right");
        menuItem.addActionListener(new PanListener(PanListener.EAST));
        menu.add(menuItem); 
        
          
    }
    // ---------------------------------------------------------------------------------------------------------------
    
    
    
    private void addComponent(Container container,
            Component component,
            GridBagConstraints gbc,
            int column, int row,
            int columnCells, int rowCells,
            int weightX, int weightY)
    {

//      how much it can grow in the X and Y directions   
        gbc.weightx = weightX;
        gbc.weighty  = weightY;

//      what row and column it starts in
        gbc.gridx = column;
        gbc.gridy = row;

//      the number of columns and rows it takes up
        gbc.gridwidth = columnCells;
        gbc.gridheight = rowCells;

        container.add(component, gbc);

        return;
    } 
    
    
    private void addComponent(Container container,
            Component component,
            GridBagConstraints gbc,
            int column, int row,
            int columnCells, int rowCells,
            int weightX, int weightY,
            int fill)
    {

        gbc.fill = fill;
        
        addComponent(container, component, gbc ,column, row, columnCells, rowCells, weightX, weightY);      
  
        return;
    } 
    
    
    public MapLayer createStaticMapLayer()
    {
        List<LatLonPoint> latLongPointList = getLatLongPointList(34, 42, -76);
        Polygon polygon = new Polygon(latLongPointList);
        
        List<Polygon> polygonList = new ArrayList<Polygon>();
        
        polygonList.add(polygon);
        
        MapLayer mapLayer = new PolygonMapLayer(polygonList);
        mapLayer.setColor(Color.RED);
        
        return mapLayer;
   
    }
    
    public MapLayer createBinaryLineMapLayer(String filePath, Color color)
    {

        BinaryGeoDataFileReader reader = new BinaryGeoDataFileReader(filePath);   
        List<LatLonPolyline> lineList = reader.readLineList();
        MapLayer mapLayer = new PolyLineMapLayer(lineList);
        mapLayer.setColor(color);

        return mapLayer;
    }
  
    
    public MapLayer createElevationMapLayer()
    { 
        String header = "MapFrame.createElevationMapLayer(): ";
        String fileName = "topography";
        //String polygonFileDirPath = "/awips/fxa/data/localizationDataSets/RHA";
         
         String dirPath = _geoDir;

         String filePath = dirPath + '/' + fileName;
         
         CodeTimer readTimer = new CodeTimer();
         readTimer.start();
     //    ElevationFileReaderSlow reader = new ElevationFileReaderSlow(filePath);
         ElevationFileReader reader = new ElevationFileReader(filePath);
          _elevationMap = reader.read();
         readTimer.stop(header + "elevation file reading took ");
       //  System.out.println("elevationMap = " + map);
         
         ElevationMapLayer mapLayer = new ElevationMapLayer(_elevationMap, false);
              
         mapLayer.setColor(Color.red);
         
         return mapLayer;
        
    }
    
    
    public MapLayer createElevationContourMapLayer(ElevationMap map)
    {        
         ElevationMapLayer mapLayer = new ElevationMapLayer(map, true);
              
         mapLayer.setColor(Color.red);
         
         return mapLayer;     
    }
    
    
    
    public MapLayer createCountyMapLayer()
    {   
        String fileName = "reg_county.bcd";
      //  String dirPath = "/awips/fxa/data/localizationDataSets/RHA"; //the normal location for this stuff
        String dirPath = _geoDir + "/chip";    
        String filePath = dirPath + '/' + fileName;
           
        MapLayer mapLayer = createBCDPolygonMapLayer(filePath, Color.white, true);
  
        return mapLayer;
      
    }
    
    public MapLayer createBCDPolygonMapLayer(String filePath, Color color)
    {
        return createBCDPolygonMapLayer(filePath, color, false);
    }
    
    public MapLayer createBCDPolygonMapLayer(String filePath, Color color, boolean setLatLonBounds)
    {          
        BCDFileReader reader = new BCDFileReader(filePath);
        List<Polygon> polygonList = reader.readPolygonList();
         
        MapLayer mapLayer = new PolygonMapLayer(polygonList);
        mapLayer.setColor(color);
        
        if (setLatLonBounds)
        {
            LatLonBounds bounds =  reader.getLatLonBounds();
            _mapCanvas.setLatLonBounds(bounds);
      //      _mapCanvas.repaint();
            
        }
        
        return mapLayer;
      
    }
    public MapLayer createStateMapLayer()
    {   
        String fileName = "conandsta.bcd";
        String dirPath = "/awips/fxa/data/";

        String filePath = dirPath + '/' + fileName;
           
        MapLayer mapLayer = createBCDPolygonMapLayer(filePath, Color.white);
        
        return mapLayer;
      
    }
    
    // ------------------------------------------------------------------------------------------------
    
    public MapLayer createQpeMapLayer(boolean shouldDrawImage)
    {   
     
        //set up the log file and the XmrgReader
        String logDirPath = "/awips/hydroapps/ob83_fwr/whfs/local/data/log/misc";
        String logFileName = "MapFrame.xmrgReader.log";
        Logger logger = new FileLogger(logDirPath + "/" + logFileName);
        XmrgReader reader = new XmrgReader(logger);   
        
     
        //get the time of the latest hour in millis
        long millisPerHour = 1000 * 60 * 60;
        long time = System.currentTimeMillis()/ millisPerHour ;
        time *= millisPerHour;
        
        //assemble the filePath
        String fileName = "chip.xmrg";
        String dirPath = "/awips/hydroapps/ob83_fwr/precip_proc/local/data/mpe/qpe";
        String filePath = dirPath + '/' + fileName;

        
        //load the grid
        XmrgGrid xmrgGrid = reader.loadGrid(time, filePath);
        XmrgGridToGridAdapter grid = new XmrgGridToGridAdapter(xmrgGrid);
        
        //create the mapLayer
        RowColToLatLonTranslator translator = new HrapRowColToLatLonConverter(xmrgGrid.getBaseRow(),
                                                                      xmrgGrid.getBaseCol());
       
        String[] colorNameArray = { "GRAY30",
                "GRAY30",
                "GRAY30",
                "DODGERBLUE1",
                "CYAN",
                "DARKGREEN",
                "GREEN",
                "GREENYELLOW",
                "YELLOW",
                "GOLD2",
                "DARKORANGE1",
                "RED",
                "RED3",
                "RED4",
                "MAGENTA1",
                "DARKORCHID",
                "DARKTURQUOISE"
        };
        
        double[] levelArray = {0.0,
                0.1,
                0.2,
                0.3,
                0.4,
                0,5,
                0.7,
                1.2,
                1.5,
                1.7,
                2.0,
                2.5,
                3.0            
        };

     
        
        ColorDeterminer colorDeterminer = new BaseColorDeterminer(colorNameArray, levelArray);
        
        MapLayer mapLayer = new GriddedMapLayer(grid, translator, colorDeterminer, shouldDrawImage);
        
        return mapLayer;
      
    }
   
    // ------------------------------------------------------------------------------------------------
   
    // ---------------------------------------------------------------------------------------------
    
    public void redrawIt()
    {
        repaint();
    }
    
    
    // ------------------------------------------------------------------------------------------------
    public static List<LatLonPoint> getLatLongPointList(double lowLat, double highLat, double lon)
    {
        List<LatLonPoint> pointList = new ArrayList<LatLonPoint>();
        
        for (double lat = lowLat; lat <= highLat; lat++)
        {
            LatLonPoint point = new LatLonPoint(lat, lon);
            pointList.add(point);
        
        }
        
        return pointList;
    }
    
    // ------------------------------------------------------------------------------------------------
    private class StateToggleButtonListener implements ActionListener
    {
        public void actionPerformed(ActionEvent event)
        {
            JCheckBoxMenuItem checkBox = (JCheckBoxMenuItem) event.getSource();
            _stateMapLayer.setShouldDraw(checkBox.isSelected());
            _mapCanvas.repaint();
        }
    }
    
    // ------------------------------------------------------------------------------------------------
    private class MapLayerToggleButtonListener implements ActionListener
    {
        private MapLayer _mapLayer = null;
        
        public MapLayerToggleButtonListener(MapLayer mapLayer)
        {
            _mapLayer = mapLayer;
        }
        
        public void actionPerformed(ActionEvent event)
        {
            JCheckBoxMenuItem checkBox = (JCheckBoxMenuItem) event.getSource();
            _mapLayer.setShouldDraw(checkBox.isSelected());
            _mapCanvas.repaint();
        }
    }
    // ------------------------------------------------------------------------------------------------
    private class CountyToggleButtonListener implements ActionListener
    {
        
        
        public void actionPerformed(ActionEvent event)
        {
            JCheckBoxMenuItem checkBox = (JCheckBoxMenuItem) event.getSource();
            _countyMapLayer.setShouldDraw(checkBox.isSelected());
            _mapCanvas.repaint();
        }
    }
    // ------------------------------------------------------------------------------------------------
    
    
    private class FrameCloseWindowListener extends WindowAdapter implements ActionListener
    {
        public void windowClosing(WindowEvent evt)
        {
            dispose();
            System.exit(0);
        }
        
        public void actionPerformed(ActionEvent event)
        {
            dispose();
            System.exit(0);
        }

    }
    // ------------------------------------------------------------------------------------------------
    private class PanListener implements ActionListener
    {
        private static final int NORTH = 0;
        private static final int SOUTH = 1;
        private static final int EAST = 2;
        private static final int WEST = 3;
         
        
        private int _direction = -1;
        
        private double _panProportion = 0.25;
        
        public PanListener(int direction)
        {
            _direction = direction;
        }
        
        public void actionPerformed(ActionEvent evt)
        {
            switch (_direction)
            {
                case  NORTH:
                {
                    _mapCanvas.pan(_panProportion, 0);
                    break;
                }
                
                case SOUTH :
                {
                    _mapCanvas.pan( - _panProportion, 0);
                    break;
                }

                case EAST :
                {
                    _mapCanvas.pan(0, _panProportion);
                    break;
                }
                
                case WEST :
                {
                    _mapCanvas.pan(0, - _panProportion);
                    break;
                }
                
            }
        }
    } //end PanListener
    
    // ------------------------------------------------------------------------------------------------

    private class ZoomListener implements ActionListener
    {
        
        private boolean shouldZoomIn = false;
        
        public ZoomListener(String  type)
        {
            if (type.equalsIgnoreCase("IN"))
            {
                shouldZoomIn = true;
            }
            else
            {
                shouldZoomIn = false;
            }
        }
    
        public void actionPerformed(ActionEvent evt)
        {
            final double zoomInFactor = 1.5;
            final double zoomOutFactor = 1.0/zoomInFactor;
            double zoomFactor = 1;
            
            double mapScaleFactor = _mapCanvas.getMapScaleFactor();
            
            if (shouldZoomIn)
            {
                zoomFactor = zoomInFactor;
            }
            else
            {
                zoomFactor = zoomOutFactor;
            }
            
            _mapCanvas.zoom(zoomFactor);
         
        }

    } //end inner class ZoomListener
    
    //-------------------------------------------------------------------------------------------------
   
     private class RecenterActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent event)
        {
           
           _mapCanvas.recenterAtXY(_pointPopUpInvoked.x, _pointPopUpInvoked.y );
  
        }
    }
    // ------------------------------------------------------------------------------------------------
     private class RecenterAndZoomListener extends MouseAdapter
     {
         public void mousePressed(MouseEvent event)
         {
             int buttonId = event.getButton();
             
             if (buttonId == MouseEvent.BUTTON1)
             {
                 System.out.println("MapFrame.RecenterListener: would be selecting now.");
             }
             else if (buttonId == MouseEvent.BUTTON2)
             {
                 _mapCanvas.recenterAtXY(event.getX(), event.getY() );
                 _mapCanvas.zoom(1.5);  
             }

         }
     }
     
     //-------------------------------------------------------------------------------------------------
  
    private class PopupListener extends MouseAdapter
    {
          
        public void mousePressed(MouseEvent event)
        {
            if (event.getButton() == MouseEvent.BUTTON3 )
            {
                 _pointPopUpInvoked = event.getPoint();
                 _popupMenu.show(_mapCanvas, _pointPopUpInvoked.x, _pointPopUpInvoked.y);
            }
            return;
        }
    }
    
    // ------------------------------------------------------------------------------------------------
    
    private class SelectStationListener extends MouseAdapter 
    {
        public void mousePressed(MouseEvent event)
        {
            if (event.getButton() == MouseEvent.BUTTON1)
            {
                StationPoint fcstPoint = findClosestStation(event.getPoint());
                if (fcstPoint != null)
                {

                    System.out.println("MapFrame.SelectStationListener:  you just selected " + 
                            fcstPoint.getId() + "." );
                }
                else
                {
                    System.out.println("MapFrame.SelectStationListener:  No close point could be found ." );
                }
            }
        }

    } //end SelectStationListener
    //-------------------------------------------------------------------------------------------------
 
    private StationPoint findClosestStation(Point clickedPoint)
    {
        StationPoint closestFcstPoint = null;
        double closestDistance = 0.0;
        
        for (StationPoint fcstPoint : _sshpFcstPointList)
        {
            Point screenPoint = _mapProjection.getScreenPoint(fcstPoint.getLatLonPoint());
            
            double distance = clickedPoint.distance(screenPoint);
           
            if ( (closestFcstPoint == null) || (distance < closestDistance) )
            {
                closestDistance = distance;
                closestFcstPoint = fcstPoint;
            }
        }
        
        return closestFcstPoint;
    }
 
    //-------------------------------------------------------------------------------------------------
  
    public static void main(String[] args)
    {
        if (args.length < 2)
        {
            System.out.println("Usage: java ohd.hseb.geomap.MapFrame <db_connection_string> <logFilePath >");
        }
        
        String connectionString = args[0];
        String logFilePath = args[1];
        Logger logger = new FileLogger(logFilePath);
        
        MapDataManager dataManager = new MapDataManager(connectionString, logger);
        
        MapFrame frame = new MapFrame(dataManager);
        
        frame.setVisible(true);
        
        frame.redrawIt();
    }
    // ------------------------------------------------------------------------------------------------
  
  
    
} //end class MapFrame
