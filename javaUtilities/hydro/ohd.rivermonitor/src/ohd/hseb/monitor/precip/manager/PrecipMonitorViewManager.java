package ohd.hseb.monitor.precip.manager;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.List;

import javax.swing.JFileChooser;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import ohd.hseb.monitor.LocationInfoColumns;
import ohd.hseb.monitor.MonitorFrame;
import ohd.hseb.monitor.MonitorMessage;
import ohd.hseb.monitor.MonitorTimeSeriesLiteManager;
import ohd.hseb.monitor.derivedcolumns.DerivedColumnsFileManager;
import ohd.hseb.monitor.manager.BaseManager;
import ohd.hseb.monitor.manager.Receiver;
import ohd.hseb.monitor.messaging.MessageSystemManager;
import ohd.hseb.monitor.messaging.MessageType;
import ohd.hseb.monitor.precip.PrecipColumns;
import ohd.hseb.monitor.precip.PrecipMonitorDataManager;
import ohd.hseb.monitor.precip.PrecipMonitorJTableRowData;
import ohd.hseb.monitor.settings.ViewSettings;
import ohd.hseb.monitor.util.HydroappsDefaultDirs;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableColumnDisplaySettings;
import ohd.hseb.util.gui.jtable.JTableManager;

public class PrecipMonitorViewManager extends BaseManager
{
    private SessionLogger _logger;

    private PrecipMonitorDataManager _precipMonitorDataManager;
    private JTableManager _precipMonitorTableManager;
    private ViewSettings _viewSettings;

    private JSplitPane _splitPane;

    private List<PrecipMonitorJTableRowData> _precipMonitorAllRowDataList;

    private String _columnNamesCurrentlyDisplayed[];
    private DerivedColumnsFileManager _derivedColumnsFileManager;
    private MonitorFrame _mainFrame;

    private  AppsDefaults _appsDefaults;

    private PrecipMonitorJTableRowData _selectedRowData;
    
    private MonitorTimeSeriesLiteManager _monitorTimeSeriesLiteManager;

    public PrecipMonitorViewManager(MonitorFrame mainFrame, MessageSystemManager msgSystemManager, SessionLogger logger, 
            PrecipMonitorDataManager precipMonitorDataManager, JSplitPane splitPane, ViewSettings viewSettings,
            DerivedColumnsFileManager derivedColumnsFileManager, MonitorTimeSeriesLiteManager monitorTimeSeriesLiteManager,
            AppsDefaults appsDefaults)
    {
        _logger = logger;
        _precipMonitorDataManager = precipMonitorDataManager;
        _splitPane = splitPane;
        _mainFrame = mainFrame;
        _viewSettings = viewSettings;

        _derivedColumnsFileManager = derivedColumnsFileManager;
        _monitorTimeSeriesLiteManager = monitorTimeSeriesLiteManager;
        _appsDefaults = appsDefaults;

        List<JTableColumnDescriptor> precipColumnsList = new PrecipColumns().getPrecipColumnsList(null);

        _derivedColumnsFileManager.getDerivedColumns();
        String derivedColumnNames[] = _derivedColumnsFileManager.getDerivedColumnNames();
        String alignmentForDerivedColumns[] = _derivedColumnsFileManager.getAlignmentForDerivedColumns();
        if(derivedColumnNames != null)
        {
            addDerivedColumnNames(precipColumnsList, derivedColumnNames, alignmentForDerivedColumns);
        }

        Dimension columnHeaderPreferredSize = new Dimension(100, 50);
        _precipMonitorTableManager = new ComplexJTableManager(precipColumnsList, _precipMonitorAllRowDataList, columnHeaderPreferredSize);
        
        setMessageSystemManager(msgSystemManager);
        _msgSystemManager.registerReceiver(new SelectViewItemReceiver(), MessageType.FILTER_SELECT_ITEM); 
        _msgSystemManager.registerReceiver(new ChangeColumnsReceiver(), MessageType.CHANGE_COLUMNS);
        _msgSystemManager.registerReceiver(new AppSortReceiver(), MessageType.APP_SORT);
        _msgSystemManager.registerReceiver(new PrecipThreatSortReceiver(), MessageType.PRECIP_THREAT_SORT);
        _msgSystemManager.registerReceiver(new ClearSortReceiver(), MessageType.CLEAR_SORT);
        _msgSystemManager.registerReceiver(new UpdateDisplayWithSettingsReceiver(), MessageType.UPDATE_DISPLAY_WITH_SETTINGS);
        _msgSystemManager.registerReceiver(new RefreshDisplayReceiver(), MessageType.REFRESH_DISPLAY);
        _msgSystemManager.registerReceiver(new SaveViewReceiver(), MessageType.CREATE_TEXT_FROM_VIEW);
        _msgSystemManager.registerReceiver(new UpdateSettingsReceiver(), MessageType.UPDATE_SETTINGS);

    }

    private void addDerivedColumnNames(List precipColumnsList, String derivedColumnNames[], String alignmentForDerivedColumns[])
    {
        for(int i=0; i < derivedColumnNames.length; i++)
        {
            precipColumnsList.add(new JTableColumnDescriptor(derivedColumnNames[i], alignmentForDerivedColumns[i], derivedColumnNames[i]));
        }
    }

    private void createTableAndApplySettings(MonitorMessage message)
    {
        String header = "PrecipMonitorViewManager.createTableAndApplySettings(): ";
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());

        _precipMonitorAllRowDataList = _precipMonitorDataManager.getPrecipMonitorRowDataList();

        _columnNamesCurrentlyDisplayed = null;
        String columnDisplaySettingsString = _viewSettings.getColumnDisplaySettings();
        JTableColumnDisplaySettings columnDisplaySettings = _precipMonitorTableManager.getTableSettings();
        columnDisplaySettings.setSettingsFromString(columnDisplaySettingsString);

        _columnNamesCurrentlyDisplayed = _precipMonitorTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        if(_columnNamesCurrentlyDisplayed == null)
        {
            _columnNamesCurrentlyDisplayed = new String[] {LocationInfoColumns.LOCATION_ID, LocationInfoColumns.GROUP_NAME};
        }

        List selectedRowDataList = _precipMonitorDataManager.getTreeFilterManager().filter(_precipMonitorAllRowDataList);
        _precipMonitorTableManager.setChangedAllRowDataList(selectedRowDataList);

        _precipMonitorTableManager.setDisplayableColumns(_columnNamesCurrentlyDisplayed, true, true);
        _precipMonitorTableManager.setPreferredSizeForJScrollPane(new Dimension(690, 645));
        
        JScrollPane tableScrollPane = _precipMonitorTableManager.getJScrollPane();
        _splitPane.setRightComponent(tableScrollPane);

        _precipMonitorTableManager.addTableListener(new TableMouseMotionListener());
        _precipMonitorTableManager.addTableListener(new TableListSelectionListener());
        _precipMonitorTableManager.addTableListener(new TableMouseListener());
    }

        
    public void refreshDisplay(MonitorMessage message)
    {
        String header = "PrecipMonitorViewManager.refreshDisplay(): ";
  
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());

        _precipMonitorAllRowDataList = _precipMonitorDataManager.getPrecipMonitorRowDataList();
        List selectedRowDataList = _precipMonitorDataManager.getTreeFilterManager().filter(_precipMonitorAllRowDataList);
        _precipMonitorTableManager.setChangedAllRowDataList(selectedRowDataList);

        _precipMonitorTableManager.refreshDisplay();

        JScrollPane tableScrollPane = _precipMonitorTableManager.getJScrollPane();

        _splitPane.setRightComponent(tableScrollPane);   
    }

    private void setSplitPaneDividerLocation()
    {
        String header = "PrecipMonitorViewManager.setSplitPaneDividerLocation(): ";
      
        int newDividerLocation =  _splitPane.getDividerLocation();
         
         _splitPane.setDividerLocation(newDividerLocation);
      
         return;
    }
    
    private void updateSettings(MonitorMessage message)
    {
        String header = "PrecipMonitorViewManager.updateSettings(): ";
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());
        updateSettings();
    }

    private void updateSettings()
    {
        String header = "PrecipMonitorViewManager.updateSettings(): ";
        JTableColumnDisplaySettings columnDisplaySettings = _precipMonitorTableManager.getTableSettings();
        String settings = columnDisplaySettings.getSettingsString();
        _viewSettings.setColumnDisplaySettings(settings);
    }

    private void selectViewItem(MonitorMessage message)
    {
        String header = "PrecipMonitorViewManager.selectViewItem(): ";
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());

        String lidOfInterest = _precipMonitorDataManager.getLidOfCurrentInterest();
        int rowToHighLight = _precipMonitorTableManager.getTheRowIndexToHighLightBasedOnValue(lidOfInterest, LocationInfoColumns.LOCATION_ID);
        System.out.println("Row to highlight:" + rowToHighLight);
        if(rowToHighLight != -1)
            _precipMonitorTableManager.selectRows(rowToHighLight, rowToHighLight);
    }

    public void printFileException(IOException exception, String header)
    {
        header = header+".printFileException(): ";
        _logger.log(header+"ERROR = " + exception.getMessage());
        exception.printStackTrace(_logger.getPrintWriter());
        _logger.log(header+"End of stack trace");
    }

    private void saveTableToFile(String fileName)
    {
        File fileHandler = new File(fileName);
        String header = "PrecipMonitorViewManager.saveTableToFile(): ";
        String displayedColumns[] = _precipMonitorTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        String rowData[][] = _precipMonitorTableManager.getDisplayRowDataArray();
        BufferedWriter out = null;
        int maxStringLen = 33;
        try
        {
            FileOutputStream fileOutput = new FileOutputStream(fileName, false);
            OutputStreamWriter outputStream = new OutputStreamWriter(fileOutput);
            out = new BufferedWriter(outputStream);
        }
        catch(IOException exception)
        {
            printFileException(exception, header);         
        }
        if(displayedColumns != null)
        {
            try
            {
                out.newLine();  
                StringBuffer columnNameStringBuffer = new StringBuffer();
                for(int i=0; i < displayedColumns.length; i++)
                {
                    columnNameStringBuffer.append(getTrimmedOrPaddedString(displayedColumns[i], maxStringLen));
                    columnNameStringBuffer.append(" |");
                }
                out.write(columnNameStringBuffer.toString());
                out.newLine();
            }
            catch(IOException exception)
            {
                printFileException(exception, header);             
            }
        }
        try
        {
            for(int i=0; i < rowData.length; i++)
            {
                out.newLine();
                StringBuffer dataStringBuffer = new StringBuffer();
                for(int j=0; j < rowData[i].length; j++)
                {
                    dataStringBuffer.append(getTrimmedOrPaddedString(rowData[i][j], maxStringLen));
                    dataStringBuffer.append(" |");
                }
                out.write(dataStringBuffer.toString());
            }
        }
        catch(IOException exception)
        {
            printFileException(exception,header);
        }

        try
        {
            out.close();
        }
        catch(IOException exception)
        {
            _logger.log(header+ "Unable to close file["+
                fileHandler.getAbsolutePath()+"]");
            printFileException(exception, header);
        }
    }

    private String getTrimmedOrPaddedString(String origString, int desiredLength)
    {
        String newString = origString;

        String formatString = "%-"+ String.valueOf(desiredLength) +"s";

        newString = String.format(formatString, origString);

        if (newString.length() > desiredLength)
        {
            newString = newString.substring(0, desiredLength);    
        }

        return newString;
    }

    private void saveView(MonitorMessage message)
    {
        String header = "PrecipMonitorViewManager.saveView(): ";
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());

        File file = null;
        JFileChooser saveFileChooser = new JFileChooser();
        String settingsDir = _appsDefaults.getToken("whfs_report_dir", 
        	HydroappsDefaultDirs.WHFS_REPORT_DIR);
        saveFileChooser.setDialogTitle("Export Table To Text File");
        saveFileChooser.setCurrentDirectory(new File(settingsDir));
        saveFileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

        int result = saveFileChooser.showSaveDialog(_mainFrame);
        if(result == JFileChooser.CANCEL_OPTION)
            file = null;
        else
            file = saveFileChooser.getSelectedFile();

        if(file != null)
        {
            saveTableToFile(file.toString());
        }
    }

    private void changeColumns(MonitorMessage message)
    {
        String header = "PrecipMonitorViewManager.changeColumns(): ";
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());

        _precipMonitorTableManager.invokeColumnSelector(_mainFrame);

        updateSettings();
    }

    private void updateSort(String[] columnNamesToSort,int[] columnSortNumber,boolean[] columnSortOrder)
    {
        _precipMonitorTableManager.setSortInfoNotBasedOnUserClicks(columnNamesToSort, columnSortOrder, columnSortNumber);
        updateSettings();
    }
    
    private void precipThreatSort(MonitorMessage message)
    {
        String header = "PrecipMonitorViewManager.precipThreatSort(): ";
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());

        clearSort();

        String columnNamesToSort[] = {PrecipColumns.PRECIP_THREAT};
        int columnSortNumber[] = {0};
        boolean columnSortOrder[] = {false};

        updateSort(columnNamesToSort, columnSortNumber, columnSortOrder);
    }

    private void appSort(MonitorMessage message)
    {
        String header = "PrecipMonitorViewManager.appSort(): ";
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());

        clearSort();

        String columnNamesToSort[] = {LocationInfoColumns.HSA, LocationInfoColumns.GROUP_ORDINAL, 
                LocationInfoColumns.LOCATION_ORDINAL};
        int columnSortNumber[] = {0,1,2};
        boolean columnSortOrder[] = {true, true, true};

        updateSort(columnNamesToSort, columnSortNumber, columnSortOrder);
    }

    private void clearSort()
    {
        _precipMonitorTableManager.clearSort();
    }

    private void clearSort(MonitorMessage message)
    {
        String header = "PrecipMonitorViewManager.clearSort(): ";
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());

        clearSort();
        updateSettings();
    }

    private void rowSelected(PrecipMonitorJTableRowData selectedRowData)
    {
        String header = "PrecipMonitorViewManager.rowSelected(): ";

        System.out.print(header);
        send(this, MessageType.VIEW_SELECT_ITEM, selectedRowData);
    }

//  -------------------------------------------------------------------------------------
    public String getToolTipForSelectedCell(String columnName, int rowIndex)
    {
        PrecipMonitorJTableRowData rowData = (PrecipMonitorJTableRowData)_precipMonitorTableManager.getSelectedRowData(rowIndex);
        String toolTip = "";
        if(columnName.equals(PrecipColumns.PRECIP_THREAT))
        {
            toolTip = rowData.getToolTipTextForPrecipThreatSummary();
        }
        if(toolTip.length() <= 12) // tooltip has "<HTML><BODY>" only
        {
            String name = rowData.getName();
            String state = rowData.getState();
            String county = rowData.getCounty();
            String hsa = rowData.getHsa();

            toolTip = "<HTML><BODY>Name:["+ name+"]\n<BR>"+
            "County: ["+county+"] State: ["+state+"] HSA: ["+hsa+"]\n<BR>";
        }
        return toolTip;
    }

    private class ChangeColumnsReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            changeColumns(message);
        }
    }

    private class ClearSortReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            clearSort(message);
        }
    }

    private class PrecipThreatSortReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            precipThreatSort(message);
        }
    }
    
    private class AppSortReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            appSort(message);
        }
    }

    private class SelectViewItemReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            selectViewItem(message);
        }
    }

    private class RefreshDisplayReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            refreshDisplay(message);
        }
    }

    private class UpdateDisplayWithSettingsReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            createTableAndApplySettings(message);
        }
    }

    private class UpdateSettingsReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            updateSettings(message);
        }
    }

    private class SaveViewReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            saveView(message);
        }
    }

    private class TableListSelectionListener implements ListSelectionListener
    {
        public void valueChanged(ListSelectionEvent e) 
        {
            if(e.getValueIsAdjusting() == false)
            {
                List selectedRowData = _precipMonitorTableManager.getSelectedRowsData();
                for(int i=0; i < selectedRowData.size(); i++)
                {
                    if(i == 0)
                    {
                        PrecipMonitorJTableRowData rowData = (PrecipMonitorJTableRowData) selectedRowData.get(i);
                        _selectedRowData = rowData;
                        rowSelected(_selectedRowData);
                    }
                }
            }
        }
    }

    private class TableMouseListener implements MouseListener
    {
        public void mouseClicked(MouseEvent e) 
        {
            //String header = "PrecipMonitorViewManager.TableMouseListener.mouseClicked(): ";
            Point p = new Point(e.getX(), e.getY());
            int rowIndex = _precipMonitorTableManager.getMousePointedRowIndex(p);
            PrecipMonitorJTableRowData rowData = (PrecipMonitorJTableRowData) _precipMonitorTableManager.getSelectedRowData(rowIndex); 

            if(e.getClickCount() == 2)
            {
                //System.out.println(header + " starting TSL for " + rowData.getLid());
                _selectedRowData = rowData;
                rowSelected(_selectedRowData);
                _monitorTimeSeriesLiteManager.displayTimeSeriesLite(_mainFrame, _selectedRowData, 1);
            }
        }
        public void mousePressed(MouseEvent e){}
        public void mouseReleased(MouseEvent e){}
        public void mouseEntered(MouseEvent e){}
        public void mouseExited(MouseEvent e){}
    }

//  -------------------------------------------------------------------------------------
    class TableMouseMotionListener implements MouseMotionListener
    {
        public void mouseDragged(MouseEvent e) 
        {
        }
        public void mouseMoved(MouseEvent e) 
        {
            Point p = new Point(e.getX(), e.getY());
            int rowIndex = _precipMonitorTableManager.getMousePointedRowIndex(p);
            int colIndex = _precipMonitorTableManager.getMousePointedColumnIndex(p);
            String columnName = _precipMonitorTableManager.getColumnNameAtIndex(colIndex);
            String toolTip="";
            if(rowIndex != -1)
            {
                toolTip = getToolTipForSelectedCell(columnName, rowIndex);
                _precipMonitorTableManager.setRowToolTipText(toolTip);
            }
        }
    }
}
