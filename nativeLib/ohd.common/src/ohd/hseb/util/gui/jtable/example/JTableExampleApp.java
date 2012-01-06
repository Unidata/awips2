package ohd.hseb.util.gui.jtable.example;

import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JScrollPane;

import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

/**
 * <p>
 * JTableExampApp - Class containing the main method for JTable Example.
 * This class shows the basic methods to be called in JTable Framework to perform the following:
 * <p>
 *     <li> Display a JTable,
 *     <li> Refresh a JTable,
 *     <li> Column Selection Feature
 *     
 * @author  rajaramv
 * 
 * 
 */

public class JTableExampleApp extends JFrame
{
    JFrame _frame ;
    JTableManager _tableManager;

    public void showApp()
    {
        _frame = new JFrame("JTable Framework Example");
        _frame.setLayout(new GridBagLayout());

        JTableExampleDataManager dataManager = new JTableExampleDataManager();

        List rowDataList = dataManager.readData();
        List columnDescriptorList = createColumnDescriptorList();

        JScrollPane tableScrollPane = createJTable(columnDescriptorList, rowDataList);
        createMenus();

        _frame.getContentPane().add(tableScrollPane);
        _frame.pack();
        _frame.setVisible(true);
    }

    /**
     * Creates a JTable using the columnDescriptorList and rowDataList
     * @param columnDescriptorList - column descriptor list containing JTableColumnDescriptor for each column in the table
     * @param rowDataList - list of JTableRowData each representing a row
     * @return JScrollPane - JScrollPane containing the JTable
     */
    public JScrollPane createJTable(List columnDescriptorList, List rowDataList)
    {
        _tableManager = new ComplexJTableManager(columnDescriptorList, rowDataList);
        _tableManager.setPreferredSizeForJScrollPane(new Dimension(680, 320));
        JScrollPane tableScrollPane = _tableManager.getJScrollPane();
        return tableScrollPane;
    }

    /**
     * Creates the JTableColumnDescriptor for each column in the table and returns it.
     * @return List - columnDescriptorList 
     */
    public List createColumnDescriptorList()
    {
        List columnDescriptorList = new ArrayList();

        columnDescriptorList.add( new JTableColumnDescriptor("Id"));
        columnDescriptorList.add( new JTableColumnDescriptor("Age"));
        columnDescriptorList.add( new JTableColumnDescriptor("BirthDate", 60, "center", "Date Of Birth"));
        columnDescriptorList.add( new JTableColumnDescriptor("Height"));
        columnDescriptorList.add( new JTableColumnDescriptor("Weight"));
        columnDescriptorList.add( new JTableColumnDescriptor("Name"));
        columnDescriptorList.add( new JTableColumnDescriptor("Married"));
        columnDescriptorList.add( new JTableColumnDescriptor("Salary", 75, "right", "Annual Salary"));

        return columnDescriptorList;
    }

    /**
     * Create the menus for column selection and refresh.
     * This is to demonstrate the column selection and refresh ability of the jtable framework. 
     *
     */
    public void createMenus()
    {
        JMenuBar menuBar = new JMenuBar();
        _frame.setJMenuBar(menuBar);

        JMenu fileMenu = new JMenu("File");
        fileMenu.setMnemonic('F');
        JMenuItem columnSelectionMenuItem = new JMenuItem("Column Selection");
        columnSelectionMenuItem.setMnemonic('C');
        fileMenu.add(columnSelectionMenuItem);

        JMenuItem refreshMenuItem = new JMenuItem("Refresh");
        refreshMenuItem.setMnemonic('R');
        fileMenu.add(refreshMenuItem);

        ChangeColumnsDisplayedMenuListener changeMenuListener = new 
        ChangeColumnsDisplayedMenuListener();
        columnSelectionMenuItem.addActionListener(changeMenuListener);

        RefreshMenuListener RefreshMenuListener = new
        RefreshMenuListener();
        refreshMenuItem.addActionListener(RefreshMenuListener);

        menuBar.add(fileMenu);

    }

    private class ChangeColumnsDisplayedMenuListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            _tableManager.invokeColumnSelector(_frame);
        }
    }

    private class RefreshMenuListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            List rowDataList = new JTableExampleDataManager().readData();
            _tableManager.setChangedAllRowDataList(rowDataList);

            _tableManager.refreshDisplay();
        }
    }

    public static void main(String args[])
    {
        new JTableExampleApp().showApp();
    }
}
