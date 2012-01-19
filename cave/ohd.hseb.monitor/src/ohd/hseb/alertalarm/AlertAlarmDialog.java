package ohd.hseb.alertalarm;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.gui.WindowResizingManager;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableManager;

public class AlertAlarmDialog extends JDialog
{
    private List _alertAlarmColumnDescriptorList = null;
    private JTableManager _alertAlarmTableManager = null;
    private AlertAlarmDataManager _alertAlarmDataMgr = null;
    private List _allRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private SessionLogger _logger = null;
    private int _previousRowToHighlight = -1;
    private JFrame _mainFrame = null;
    private JMenuBar _menuBar;
    private JMenu  _fileMenu;
    private JMenuItem _selectColumnsMenuItem;

    public AlertAlarmDialog(JFrame mainFrame, AlertAlarmDataManager alertAlarmDataMgr, SessionLogger logger)
    {
        super(mainFrame, true);
        _mainFrame = mainFrame;
        _logger = logger;
        this.setTitle("AlertAlarm");
        this.getContentPane().setLayout(new GridBagLayout());
        _alertAlarmDataMgr = alertAlarmDataMgr;
        initialize();
        Dimension dim = new Dimension(1040,560);
        new WindowResizingManager(this, dim, dim);
        this.pack();
        this.setLocation(25, 25);
    }

    public void showAlertAlarmDialog(String lid)
    {
        _allRowDataList = _alertAlarmDataMgr.readDataFromAlertAlarmVal();
        _alertAlarmTableManager.setChangedAllRowDataList(_allRowDataList);
        _alertAlarmTableManager.refreshDisplay();

        if(lid != null)
        {
            _alertAlarmTableManager.deselectRows(_previousRowToHighlight, _previousRowToHighlight);
            int rowToHighLight = _alertAlarmTableManager.getTheRowIndexToHighLightBasedOnValue(lid);
            if(rowToHighLight != -1)
            {
                System.out.println("Row to highlight:"+ rowToHighLight);
                _alertAlarmTableManager.selectRows(rowToHighLight, rowToHighLight);
                _previousRowToHighlight = rowToHighLight;
            }
        }
        this.setVisible(true);
    }

    private void initialize()
    {
        _alertAlarmColumnDescriptorList = new AlertAlarmColumns().getAlertAlarmColumnsList(_alertAlarmColumnDescriptorList);
        _allRowDataList = _alertAlarmDataMgr.readDataFromAlertAlarmVal();
        _alertAlarmTableManager = new ComplexJTableManager(_alertAlarmColumnDescriptorList, _allRowDataList);
        String columnsSelected[] = _alertAlarmTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _alertAlarmTableManager.setDisplayableColumns(columnsSelected, true, true);
        _alertAlarmTableManager.setPreferredSizeForJScrollPane(new Dimension(1020, 500));
        _tableScrollPane = _alertAlarmTableManager.getJScrollPane();

        JPanel tableScrollPanePanel = new JPanel(); 
        tableScrollPanePanel.add(_tableScrollPane);
        tableScrollPanePanel.setMinimumSize(new Dimension(1030,510));
        JPanel tablePanel = new JPanel();
        tablePanel.add(tableScrollPanePanel);

        _menuBar = new JMenuBar();
        this.setJMenuBar(_menuBar);

        _fileMenu = new JMenu("File");
        _selectColumnsMenuItem = new JMenuItem("Select Columns ...");
        _selectColumnsMenuItem.setMnemonic('S');
        _fileMenu.add(_selectColumnsMenuItem);

        _menuBar.add(_fileMenu);

        ChangeColumnsDisplayedMenuListener changeMenuListener = new 
        ChangeColumnsDisplayedMenuListener();
        _selectColumnsMenuItem.addActionListener(changeMenuListener);

        Container contentPane = this.getContentPane();
        contentPane.add(tablePanel);
    }

    private class ChangeColumnsDisplayedMenuListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            _alertAlarmTableManager.invokeColumnSelector(_mainFrame);

        }
    }

}
