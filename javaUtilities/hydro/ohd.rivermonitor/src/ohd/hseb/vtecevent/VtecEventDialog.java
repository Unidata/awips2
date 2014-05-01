package ohd.hseb.vtecevent;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.util.List;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.gui.WindowResizingManager;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableManager;

public class VtecEventDialog extends JDialog 
{
    private List _vtecEventColumnDescriptorList = null;
    private JTableManager _vtecEventTableManager = null;
    private VtecEventDataManager _vtecDataMgr = null;
    private List _allRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private SessionLogger _logger;
    private int _previousRowToHighlight = -1;

    public VtecEventDialog(JFrame mainFrame, VtecEventDataManager vtecDataMgr, SessionLogger logger)
    {
        super(mainFrame, true);
        _logger = logger;
        this.setTitle("Valid Time Event Code (VTEC) History");
        this.getContentPane().setLayout(new GridBagLayout());
        _vtecDataMgr = vtecDataMgr;
        initialize();
        Dimension dim;
        dim = new Dimension(1250,560);
        new WindowResizingManager(this, dim, dim);
        this.pack();
        this.setLocation(25, 25);
    }

    public void showVtecEventDialog(String geoId)
    {
        _allRowDataList = _vtecDataMgr.readDataFromVtecEvent();
        _vtecEventTableManager.setChangedAllRowDataList(_allRowDataList);
        _vtecEventTableManager.refreshDisplay();

        if(geoId != null)
        {
            _vtecEventTableManager.deselectRows(_previousRowToHighlight, _previousRowToHighlight);
            int rowToHighLight = _vtecEventTableManager.getTheRowIndexToHighLightBasedOnValue(geoId);
            if(rowToHighLight != -1)
            {
                _vtecEventTableManager.selectRows(rowToHighLight, rowToHighLight);
                _previousRowToHighlight = rowToHighLight;
            }
        }
        this.setVisible(true);
    }

    private void initialize()
    {
        _vtecEventColumnDescriptorList = new VtecEventColumns().getVtecEventColumnsList(_vtecEventColumnDescriptorList);
        _allRowDataList = _vtecDataMgr.readDataFromVtecEvent();

        _vtecEventTableManager = new ComplexJTableManager(_vtecEventColumnDescriptorList, _allRowDataList);
        String columnsSelected[] = _vtecEventTableManager.getColumnNamesThatAreCurrentlyDisplayed();

        _vtecEventTableManager.setDisplayableColumns(columnsSelected, true, true);

        _vtecEventTableManager.setPreferredSizeForJScrollPane(new Dimension(1230, 500));

        _tableScrollPane = _vtecEventTableManager.getJScrollPane();

        JPanel tableScrollPanePanel = new JPanel(); 
        tableScrollPanePanel.add(_tableScrollPane);

        tableScrollPanePanel.setMinimumSize(new Dimension(1240,510));

        JPanel tablePanel = new JPanel();
        tablePanel.add(tableScrollPanePanel);

        Container contentPane = this.getContentPane();
        contentPane.add(tablePanel);
    }

}
