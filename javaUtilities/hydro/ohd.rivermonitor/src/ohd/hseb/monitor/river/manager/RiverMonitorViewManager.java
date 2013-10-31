package ohd.hseb.monitor.river.manager;

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

import ohd.hseb.db.DbTable;
import ohd.hseb.ihfsdb.generated.VTECeventRecord;
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
import ohd.hseb.monitor.river.RiverColumns;
import ohd.hseb.monitor.river.RiverMonitorDataManager;
import ohd.hseb.monitor.river.RiverMonitorJTableRowData;
import ohd.hseb.monitor.settings.ViewSettings;
import ohd.hseb.monitor.util.HydroappsDefaultDirs;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableColumnDisplaySettings;
import ohd.hseb.util.gui.jtable.JTableManager;

public class RiverMonitorViewManager extends BaseManager
{
    private SessionLogger _logger;

    private RiverMonitorDataManager _riverMonitorDataManager;

    private JTableManager _riverMonitorTableManager;

    private ViewSettings _viewSettings;

    private JSplitPane _splitPane;

    private List<RiverMonitorJTableRowData> _riverMonitorAllRowDataList;

    private String _columnNamesCurrentlyDisplayed[];

    private DerivedColumnsFileManager _derivedColumnsFileManager;

    private MonitorFrame _mainFrame;

    private RiverMonitorJTableRowData _selectedRowData;

    private MonitorTimeSeriesLiteManager _monitorTimeSeriesLiteManager;

    private AppsDefaults _appsDefaults;

    public RiverMonitorViewManager(MonitorFrame mainFrame,
            MessageSystemManager msgSystemManager, SessionLogger logger,
            RiverMonitorDataManager riverMonitorDataManager,
            JSplitPane splitPane, ViewSettings viewSettings,
            DerivedColumnsFileManager derivedColumnsFileManager,
            MonitorTimeSeriesLiteManager monitorTimeSeriesLiteManager,
            AppsDefaults appsDefaults)
    {
        _logger = logger;
        _riverMonitorDataManager = riverMonitorDataManager;
        _splitPane = splitPane;
        _mainFrame = mainFrame;
        _viewSettings = viewSettings;

        _derivedColumnsFileManager = derivedColumnsFileManager;
        _monitorTimeSeriesLiteManager = monitorTimeSeriesLiteManager;
        _appsDefaults = appsDefaults;

        List<JTableColumnDescriptor> riverColumnsList = new RiverColumns()
                .getRiverColumnsList(null);

        _derivedColumnsFileManager.getDerivedColumns();
        String derivedColumnNames[] = _derivedColumnsFileManager
                .getDerivedColumnNames();
        String alignmentForDerivedColumns[] = _derivedColumnsFileManager
                .getAlignmentForDerivedColumns();
        if (derivedColumnNames != null)
        {
            addDerivedColumnNames(riverColumnsList, derivedColumnNames,
                    alignmentForDerivedColumns);
        }

        Dimension columnHeaderPreferredSize = new Dimension(100, 50);
        _riverMonitorTableManager = new ComplexJTableManager(riverColumnsList,
                _riverMonitorAllRowDataList, columnHeaderPreferredSize);

        setMessageSystemManager(msgSystemManager);
        _msgSystemManager.registerReceiver(new SelectViewItemReceiver(),
                MessageType.FILTER_SELECT_ITEM);
        _msgSystemManager.registerReceiver(new ChangeColumnsReceiver(),
                MessageType.CHANGE_COLUMNS);
        _msgSystemManager.registerReceiver(new AppSortReceiver(),
                MessageType.APP_SORT);
        _msgSystemManager.registerReceiver(new RiverThreatSortReceiver(),
                MessageType.RIVER_THREAT_SORT);
        _msgSystemManager.registerReceiver(new PrecipThreatSortReceiver(),
                MessageType.PRECIP_THREAT_SORT);
        _msgSystemManager.registerReceiver(new ClearSortReceiver(),
                MessageType.CLEAR_SORT);
        _msgSystemManager.registerReceiver(
                new UpdateDisplayWithSettingsReceiver(),
                MessageType.UPDATE_DISPLAY_WITH_SETTINGS);
        _msgSystemManager.registerReceiver(new RefreshDisplayReceiver(),
                MessageType.REFRESH_DISPLAY);
        _msgSystemManager.registerReceiver(new SaveViewReceiver(),
                MessageType.CREATE_TEXT_FROM_VIEW);
        _msgSystemManager.registerReceiver(new UpdateSettingsReceiver(),
                MessageType.UPDATE_SETTINGS);

    }

    private void addDerivedColumnNames(List riverColumnsList,
            String derivedColumnNames[], String alignmentForDerivedColumns[])
    {
        for (int i = 0; i < derivedColumnNames.length; i++)
        {
            riverColumnsList.add(new JTableColumnDescriptor(
                    derivedColumnNames[i], alignmentForDerivedColumns[i],
                    derivedColumnNames[i]));
        }
    }

    private void createTableAndApplySettings(MonitorMessage message)
    {
        String header = "RiverViewManager.createTableAndApplySettings(): ";
        System.out.println(header + " Invoked" + "...Source:"
                + message.getMessageSource() + " Type:"
                + message.getMessageType());

        _riverMonitorAllRowDataList = _riverMonitorDataManager
                .getRiverMonitorRowDataList();

        _columnNamesCurrentlyDisplayed = null;
        String columnDisplaySettingsString = _viewSettings
                .getColumnDisplaySettings();
        JTableColumnDisplaySettings columnDisplaySettings = _riverMonitorTableManager
                .getTableSettings();
        columnDisplaySettings
                .setSettingsFromString(columnDisplaySettingsString);

        _columnNamesCurrentlyDisplayed = _riverMonitorTableManager
                .getColumnNamesThatAreCurrentlyDisplayed();
        if (_columnNamesCurrentlyDisplayed == null)
        {
            _columnNamesCurrentlyDisplayed = new String[] {
                    LocationInfoColumns.LOCATION_ID,
                    LocationInfoColumns.GROUP_NAME };
        }

        List selectedRowDataList = _riverMonitorDataManager
                .getTreeFilterManager().filter(_riverMonitorAllRowDataList);
        _riverMonitorTableManager.setChangedAllRowDataList(selectedRowDataList);

        _riverMonitorTableManager.setDisplayableColumns(
                _columnNamesCurrentlyDisplayed, true, true);
        _riverMonitorTableManager.setPreferredSizeForJScrollPane(new Dimension(
                690, 645));

        JScrollPane tableScrollPane = _riverMonitorTableManager
                .getJScrollPane();

        _splitPane.setRightComponent(tableScrollPane);
     
        _riverMonitorTableManager
                .addTableListener(new TableMouseMotionListener());
        _riverMonitorTableManager
                .addTableListener(new TableListSelectionListener());
        _riverMonitorTableManager.addTableListener(new TableMouseListener());
    }

    public void refreshDisplay(MonitorMessage message)
    {
        String header = "RiverMonitorViewManager.refreshDisplay(): ";
        
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());

        _riverMonitorAllRowDataList = _riverMonitorDataManager.getRiverMonitorRowDataList();
        List selectedRowDataList = _riverMonitorDataManager.getTreeFilterManager().filter(_riverMonitorAllRowDataList);
        _riverMonitorTableManager.setChangedAllRowDataList(selectedRowDataList);

        _riverMonitorTableManager.refreshDisplay();

        JScrollPane tableScrollPane = _riverMonitorTableManager.getJScrollPane();
             
        _splitPane.setRightComponent(tableScrollPane);

    }

    private void updateSettings(MonitorMessage message)
    {
        String header = "RiverViewManager.selectViewItem(): ";
        System.out.println(header + " Invoked" + "...Source:"
                + message.getMessageSource() + " Type:"
                + message.getMessageType());
        updateSettings();
    }

    private void updateSettings()
    {
        JTableColumnDisplaySettings columnDisplaySettings = _riverMonitorTableManager
                .getTableSettings();
        String settings = columnDisplaySettings.getSettingsString();
        _viewSettings.setColumnDisplaySettings(settings);
    }

    private void selectViewItem(MonitorMessage message)
    {
        String header = "RiverViewManager.selectViewItem(): ";
        System.out.println(header + " Invoked" + "...Source:"
                + message.getMessageSource() + " Type:"
                + message.getMessageType());

        String lidOfInterest = _riverMonitorDataManager
                .getLidOfCurrentInterest();
        int rowToHighLight = _riverMonitorTableManager
                .getTheRowIndexToHighLightBasedOnValue(lidOfInterest,
                        LocationInfoColumns.LOCATION_ID);
        System.out.println("Row to highlight:" + rowToHighLight);
        if (rowToHighLight != -1)
            _riverMonitorTableManager
                    .selectRows(rowToHighLight, rowToHighLight);
    }

    public void printFileException(IOException exception, String header)
    {
        header = header + ".printFileException(): ";
        _logger.log(header + "ERROR = " + exception.getMessage());
        exception.printStackTrace(_logger.getPrintWriter());
        _logger.log(header + "End of stack trace");
    }

    private void saveTableToFile(String fileName)
    {
        File fileHandler = new File(fileName);
        String header = "RiverMonitorViewManager.saveTableToFile(): ";
        String displayedColumns[] = _riverMonitorTableManager
                .getColumnNamesThatAreCurrentlyDisplayed();
        String rowData[][] = _riverMonitorTableManager.getDisplayRowDataArray();
        BufferedWriter out = null;
        int maxStringLen = 33;
        try
        {
            FileOutputStream fileOutput = new FileOutputStream(fileName, false);
            OutputStreamWriter outputStream = new OutputStreamWriter(fileOutput);
            out = new BufferedWriter(outputStream);
        }
        catch (IOException exception)
        {
            printFileException(exception, header);
        }
        if (displayedColumns != null)
        {
            try
            {
                out.newLine();
                StringBuffer columnNameStringBuffer = new StringBuffer();
                for (int i = 0; i < displayedColumns.length; i++)
                {
                    columnNameStringBuffer.append(getTrimmedOrPaddedString(
                            displayedColumns[i], maxStringLen));
                    columnNameStringBuffer.append(" |");
                }
                out.write(columnNameStringBuffer.toString());
                out.newLine();
            }
            catch (IOException exception)
            {
                printFileException(exception, header);
            }
        }
        try
        {
            for (int i = 0; i < rowData.length; i++)
            {
                out.newLine();
                StringBuffer dataStringBuffer = new StringBuffer();
                for (int j = 0; j < rowData[i].length; j++)
                {
                    dataStringBuffer.append(getTrimmedOrPaddedString(
                            rowData[i][j], maxStringLen));
                    dataStringBuffer.append(" |");
                }
                out.write(dataStringBuffer.toString());
            }
        }
        catch (IOException exception)
        {
            printFileException(exception, header);
        }

        try
        {
            out.close();
        }
        catch (IOException exception)
        {
            _logger.log(header + "Unable to close file["
                    + fileHandler.getAbsolutePath() + "]");
            printFileException(exception, header);
        }
    }

    private String getTrimmedOrPaddedString(String origString, int desiredLength)
    {
        String newString = origString;

        String formatString = "%-" + String.valueOf(desiredLength) + "s";

        newString = String.format(formatString, origString);

        if (newString.length() > desiredLength)
        {
            newString = newString.substring(0, desiredLength);
        }

        return newString;
    }

    private void saveView(MonitorMessage message)
    {
        String header = "RiverMonitorViewManager.saveView(): ";
        System.out.println(header + " Invoked" + "...Source:"
                + message.getMessageSource() + " Type:"
                + message.getMessageType());

        File file = null;
        JFileChooser saveFileChooser = new JFileChooser();
        String settingsDir = _appsDefaults.getToken("whfs_report_dir",
        	HydroappsDefaultDirs.WHFS_REPORT_DIR);
        saveFileChooser.setDialogTitle("Export Table To Text File");
        saveFileChooser.setCurrentDirectory(new File(settingsDir));
        saveFileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

        int result = saveFileChooser.showSaveDialog(_mainFrame);
        if (result == JFileChooser.CANCEL_OPTION)
            file = null;
        else
            file = saveFileChooser.getSelectedFile();

        if (file != null)
        {
            saveTableToFile(file.toString());
        }
    }

    private void changeColumns(MonitorMessage message)
    {
        String header = "RiverMonitorViewManager.changeColumns(): ";
        System.out.println(header + " Invoked" + "...Source:"
                + message.getMessageSource() + " Type:"
                + message.getMessageType());

        _riverMonitorTableManager.invokeColumnSelector(_mainFrame);

        updateSettings();
    }

    private void updateSort(String[] columnNamesToSort, int[] columnSortNumber,
            boolean[] columnSortOrder)
    {
        _riverMonitorTableManager.setSortInfoNotBasedOnUserClicks(
                columnNamesToSort, columnSortOrder, columnSortNumber);
        updateSettings();
    }

    private void riverThreatSort(MonitorMessage message)
    {
        String header = "RiverMonitorViewManager.riverThreatSort(): ";
        System.out.println(header + " Invoked" + "...Source:"
                + message.getMessageSource() + " Type:"
                + message.getMessageType());

        clearSort();

        String columnNamesToSort[] = { RiverColumns.THREAT };
        int columnSortNumber[] = { 0 };
        boolean columnSortOrder[] = { false };

        updateSort(columnNamesToSort, columnSortNumber, columnSortOrder);
    }

    private void precipThreatSort(MonitorMessage message)
    {
        String header = "RiverMonitorViewManager.precipThreatSort(): ";
        System.out.println(header + " Invoked" + "...Source:"
                + message.getMessageSource() + " Type:"
                + message.getMessageType());

        clearSort();

        String columnNamesToSort[] = { PrecipColumns.PRECIP_THREAT };
        int columnSortNumber[] = { 0 };
        boolean columnSortOrder[] = { false };

        updateSort(columnNamesToSort, columnSortNumber, columnSortOrder);
    }

    private void appSort(MonitorMessage message)
    {
        String header = "RiverMonitorViewManager.appSort(): ";
        System.out.println(header + " Invoked" + "...Source:"
                + message.getMessageSource() + " Type:"
                + message.getMessageType());

        clearSort();

        String columnNamesToSort[] = { LocationInfoColumns.HSA,
                LocationInfoColumns.GROUP_ORDINAL,
                LocationInfoColumns.LOCATION_ORDINAL };
        int columnSortNumber[] = { 0, 1, 2 };
        boolean columnSortOrder[] = { true, true, true };

        updateSort(columnNamesToSort, columnSortNumber, columnSortOrder);
    }

    private void clearSort()
    {
        _riverMonitorTableManager.clearSort();
    }

    private void clearSort(MonitorMessage message)
    {
        String header = "RiverMonitorViewManager.clearSort(): ";
        System.out.println(header + " Invoked" + "...Source:"
                + message.getMessageSource() + " Type:"
                + message.getMessageType());

        clearSort();
        updateSettings();
    }

    private void rowSelected(RiverMonitorJTableRowData selectedRowData)
    {
        String header = "RiverMonitorViewManager.rowSelected(): ";

        System.out.print(header);
        send(this, MessageType.VIEW_SELECT_ITEM, selectedRowData);
    }

    // -------------------------------------------------------------------------------------
    public String getToolTipForSelectedCell(String columnName, int rowIndex)
    {
        RiverMonitorJTableRowData rowData = (RiverMonitorJTableRowData) _riverMonitorTableManager
                .getSelectedRowData(rowIndex);
        String toolTip = "";
        VTECeventRecord record = null;

        if (columnName.equals(RiverColumns.UGC_EXPIRE_TIME))
        {
            record = rowData.getVtecEventRecordForExpireTime();
        }
        else if (columnName.equals(RiverColumns.EVENT_END_TIME)
                || columnName.equals(RiverColumns.VTEC_SUMMARY))
        {
            record = rowData.getVtecEventRecordForEndTime();
        }

        if (record != null)
        {
            long missingValue = DbTable.getNullLong();
            String beginTime = rowData.getStringValue(record.getBegintime(),
                    missingValue);
            String endTime = rowData.getStringValue(record.getEndtime(),
                    missingValue);
            String productTime = rowData.getStringValue(
                    record.getProducttime(), missingValue);
            String expireTime = rowData.getStringValue(record.getExpiretime(),
                    missingValue);

            toolTip = "<HTML><BODY>Action: [" + record.getAction()
                    + "]  Signif: [" + record.getSignif() + "]\n<BR>"
                    + "BeginTime: [" + beginTime + "]  EndTime: [" + endTime
                    + "]\n<BR>" + "ProductTime: [" + productTime
                    + "]  ExpireTime: [" + expireTime + "]\n<BR></BODY></HTML>";
        }
        else if (columnName.equals(RiverColumns.THREAT))
        {
            toolTip = rowData.getToolTipTextForThreatSummary();
        }
        else if (columnName.equals(PrecipColumns.PRECIP_THREAT))
        {
            toolTip = rowData.getToolTipTextForPrecipThreatSummary();
        }
        else if (columnName.equals(RiverColumns.ALERT_ALARM))
        {
            toolTip = rowData.getToolTipTextForAlertAlarmSummary();
        }

        if (toolTip.length() <= 12) // tooltip has "<HTML><BODY>" only
        {
            String name = rowData.getName();
            String state = rowData.getState();
            String county = rowData.getCounty();
            String hsa = rowData.getHsa();
            String stream = rowData.getStream();
            String bankfull = rowData.getDataValue(RiverColumns.BANK_FULL);
            String actionStage = rowData.getDataValue(RiverColumns.ACT_STG);
            String floodStage = rowData.getDataValue(RiverColumns.FLD_STG);
            String actionFlow = rowData.getDataValue(RiverColumns.ACT_FLOW);
            String floodFlow = rowData.getDataValue(RiverColumns.FLD_FLOW);
            String modStage = rowData.getDataValue(RiverColumns.MOD_STG);
            String minStage = rowData.getDataValue(RiverColumns.MIN_STG);
            String majStage = rowData.getDataValue(RiverColumns.MAJ_STG);

            toolTip = "<HTML><BODY>Name:[" + name + "]\n<BR>" + "County: ["
                    + county + "] State: [" + state + "] HSA: [" + hsa
                    + "]\n<BR>" + "Stream: [" + stream + "] BankFull: ["
                    + bankfull + "]\n<BR>" + "ActionStg: [" + actionStage
                    + "] FloodStg: [" + floodStage + "]\n<BR>"
                    + "ActionFlow: [" + actionFlow + "] FloodFlow: ["
                    + floodFlow + "]\n<BR>" + "MinStg: [" + minStage
                    + "] ModStg: [" + modStage + "] MajStg: [" + majStage
                    + "]\n<BR>";
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

    private class RiverThreatSortReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            riverThreatSort(message);
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
            if (e.getValueIsAdjusting() == false)
            {
                List selectedRowData = _riverMonitorTableManager
                        .getSelectedRowsData();
                for (int i = 0; i < selectedRowData.size(); i++)
                {
                    if (i == 0)
                    {
                        RiverMonitorJTableRowData rowData = (RiverMonitorJTableRowData) selectedRowData
                                .get(i);
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
            // String header =
            // "RiverMonitorViewManager.TableMouseListener.mouseClicked(): ";
            Point p = new Point(e.getX(), e.getY());
            int rowIndex = _riverMonitorTableManager.getMousePointedRowIndex(p);
            RiverMonitorJTableRowData rowData = (RiverMonitorJTableRowData) _riverMonitorTableManager
                    .getSelectedRowData(rowIndex);

            if (e.getClickCount() == 2)
            {
                // System.out.println(header + " starting TSL for " +
                // rowData.getLid());
                _selectedRowData = rowData;
                rowSelected(_selectedRowData);
                _monitorTimeSeriesLiteManager.displayTimeSeriesLite(_mainFrame,
                        _selectedRowData);
            }
        }

        public void mousePressed(MouseEvent e)
        {
        }

        public void mouseReleased(MouseEvent e)
        {
        }

        public void mouseEntered(MouseEvent e)
        {
        }

        public void mouseExited(MouseEvent e)
        {
        }
    }

    // -------------------------------------------------------------------------------------
    class TableMouseMotionListener implements MouseMotionListener
    {
        public void mouseDragged(MouseEvent e)
        {
        }

        public void mouseMoved(MouseEvent e)
        {
            Point p = new Point(e.getX(), e.getY());
            int rowIndex = _riverMonitorTableManager.getMousePointedRowIndex(p);
            int colIndex = _riverMonitorTableManager
                    .getMousePointedColumnIndex(p);
            String columnName = _riverMonitorTableManager
                    .getColumnNameAtIndex(colIndex);
            String toolTip = "";
            if (rowIndex != -1)
            {
                toolTip = getToolTipForSelectedCell(columnName, rowIndex);
                _riverMonitorTableManager.setRowToolTipText(toolTip);
            }
        }
    }

}
