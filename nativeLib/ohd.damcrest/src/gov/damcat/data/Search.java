package gov.damcat.data;

// import com.bbn.openmap.layer.shape.*;
// import com.bbn.openmap.layer.location.*;
import javax.swing.*;
import javax.swing.table.*;
import javax.swing.event.*;
import javax.swing.text.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;

import gov.dambreak.smpdbk.*;
import gov.dambreak.util.*;

/**
 * This class allows the user to search for dams in DAMCAT using a variety of parameters.
 * After finding the correct dam, the user may view / edit that dam's information using
 * the EditDam class.
 */
public class Search extends JFrame implements ActionListener, MouseListener, ListSelectionListener {
    
    /**
     * DAMCAT_Search constructor comment.
     */
    
    class ComboBoxFrame extends JFrame implements ActionListener {
        private JComboBox states;
        private JButton okButton;
        private JButton cancelButton;
        
        private String stateNames[] =
        {
                "AL",
                "AK",
                "AZ",
                "AR",
                "CA",
                "CO",
                "CT",
                "DE",
                "DC",
                "FL",
                "GA",
                "HI",
                "IA",
                "ID",
                "IL",
                "IN",
                "KS",
                "KY",
                "LA",
                "ME",
                "MD",
                "MA",
                "MI",
                "MN",
                "MS",
                "MO",
                "MT",
                "NE",
                "NV",
                "NH",
                "NJ",
                "NM",
                "NY",
                "NC",
                "ND",
                "OH",
                "OK",
                "OR",
                "PA",
                "RI",
                "SC",
                "SD",
                "TN",
                "TX",
                "UT",
                "VT",
                "VA",
                "WA",
                "WI",
                "WV",
        "WY" };
        public void actionPerformed(ActionEvent evt)
        {
            if (evt.getSource() == cancelButton)
            {
                setVisible(false);
            }
            if (evt.getSource() == okButton)
            {
                if(buttonCloneDam == true)
                {
                    fillClonedDam();
                    setVisible(false);
                    buttonCloneDam = false;
                }
                else
                {
                    fillCreatedDam();
                    setVisible(false);
                }
            }
        }
        public ComboBoxFrame() 
        {
            setTitle("Choose a State");
            setSize(300, 200);
            setLocation(400, 200);
            Color foreground = new Color( 0,0,0 );
            
            states = new JComboBox(stateNames);
            states.setMaximumRowCount(5);
            inputState = "AL";
            states.addItemListener(
                    new ItemListener() {
                        public void itemStateChanged (ItemEvent e)
                        {  
                            if (e.getStateChange() == ItemEvent.SELECTED)
                            {
                                inputState = (String)states.getItemAt(states.getSelectedIndex());
                            }
                        }
                    });    
            
            GridBagLayout gbl = new GridBagLayout();
            GridBagConstraints constr = new GridBagConstraints();
            
            Container c = getContentPane();
            c.setLayout(gbl);
            
            JLabel lb1 = new JLabel("Please choose a state ");
            lb1.setForeground(foreground);
            
            JLabel lb2 = new JLabel("where a new dam will be created:");
            lb2.setForeground(foreground);
            
            constr.gridx = 0;
            constr.gridy = 1;
            constr.gridwidth = 1;
            constr.gridheight = 1;
            constr.weightx = 0;
            constr.weighty = 0;
            constr.anchor = GridBagConstraints.WEST;
            gbl.setConstraints(lb1, constr);
            
            c.add(lb1);
            
            constr.gridx = 0;
            constr.gridy = 2;
            constr.gridwidth = 1;
            constr.gridheight = 1;
            constr.weightx = 0;
            constr.weighty = 0;
            gbl.setConstraints(lb2, constr);
            constr.anchor = GridBagConstraints.WEST;
            c.add(lb2);
            
            constr.gridx = 1;
            constr.gridy = 1;
            constr.gridwidth = 1;
            constr.gridheight = 5;
            constr.weightx = 0;
            constr.weighty = 0;
            constr.insets = new Insets(0,20,0,0);
            gbl.setConstraints(states, constr);
            c.add(states);
            
            okButton = new JButton("    OK    ");
            okButton.setForeground(foreground);
            
            cancelButton = new JButton("Cancel");
            cancelButton.setForeground(foreground);
            
            constr.gridx = 0;
            constr.gridy = 6;
            constr.gridwidth = 1;
            constr.gridheight = 1;
            constr.weightx = 0;
            constr.weighty = 0;
            constr.insets = new Insets(80,0,0,0);
            constr.anchor = GridBagConstraints.WEST;
            gbl.setConstraints(okButton, constr); 
            c.add(okButton);
            okButton.addActionListener(this);
            
            constr.gridx = 1;
            constr.gridy = 6;
            constr.gridwidth = 1;
            constr.gridheight = 1;
            constr.weightx = 0;
            constr.weighty = 0;
            constr.insets = new Insets(80,0,0,0);
            constr.anchor = GridBagConstraints.CENTER;
            gbl.setConstraints(cancelButton, constr);
            
            c.add (cancelButton);
            cancelButton.addActionListener(this);
            
            
            // show();
            this.setVisible( true );
            
        }
        
    }
    private JPanel ivjJFrameContentPane = null;
    private int numRestrictions = 1;
    private boolean hasSpawned = false;
    IvjEventHandler ivjEventHandler = new IvjEventHandler();
    private JComboBox ivjComboRestriction1 = null;
    private JComboBox ivjComboRestriction2 = null;
    private JComboBox ivjComboRestriction3 = null;
    private JTextField ivjTextRestriction1 = null;
    private JTextField ivjTextRestriction2 = null;
    private JTextField ivjTextRestriction3 = null;
    private JButton ivjButtonClose = null;
    private ArrayList searchResults = null;
    private DBAccess dbAccess = null;
    private JButton ivjButtonClear = null;
    private JButton ivjButtonSearch = null;
    private JComboBox ivjComboOperator1 = null;
    private JComboBox ivjComboOperator2 = null;
    private JComboBox ivjComboOperator3 = null;
    private JLabel ivjLabelBetween1 = null;
    private JLabel ivjLabelBetween2 = null;
    private JLabel ivjLabelBetween3 = null;
    private JTextField ivjTextRestrictionBetween1_1 = null;
    private JTextField ivjTextRestrictionBetween1_2 = null;
    private JTextField ivjTextRestrictionBetween2_1 = null;
    private JTextField ivjTextRestrictionBetween2_2 = null;
    private JTextField ivjTextRestrictionBetween3_1 = null;
    private JTextField ivjTextRestrictionBetween3_2 = null;
    private JButton ivjButtonViewEdit = null;
    private JScrollPane ivjScrollDam = null;
    private JTable ivjTableDam = null;
    private TableColumn ivjTableColumn1 = null;
    private TableColumn ivjTableColumn2 = null;
    private TableColumn ivjTableColumn3 = null;
    private TableColumn ivjTableColumn4 = null;
    private TableColumn ivjTableColumn5 = null;
    private JPanel ivjPanelRestrictions = null;
    private JPanel ivjPanelResults = null;
    private BorderLayout ivjPanelResultsBorderLayout = null;
    private JButton ivjButtonExport = null;
    private JPanel ivjPanelDamButtons = null;
    private FlowLayout ivjPanelDamButtonsFlowLayout = null;
    private JPanel ivjPanelSearchButtons = null;
    private JPanel ivjPanelTop = null;
    private JButton ivjButtonFewer = null;
    private JButton ivjButtonMore = null;
    private JButton ivjButtonListAll = null;
    private JButton ivjButtonCreateDam = null;
    private JButton ivjButtonPrint = null;
    private JButton ivjButtonCloneDam = null;
    private JButton ivjButtonDeleteADam = null;
    private boolean buttonCloneDam = false;
    private String inputState = "AL";
    private JButton ivjButtonPrerun = null;
    
    private DamInfo searchArray[] = null;
    
    class IvjEventHandler implements java.awt.event.ActionListener {
        public void actionPerformed(java.awt.event.ActionEvent e) {
            if (e.getSource() == Search.this.getButtonMore()) 
                connEtoC1(e);
            if (e.getSource() == Search.this.getButtonFewer()) 
                connEtoC2(e);
            if (e.getSource() == Search.this.getButtonClose()) 
                connEtoC3(e);
            if (e.getSource() == Search.this.getButtonSearch()) 
                connEtoC4(e);
            if (e.getSource() == Search.this.getButtonClear()) 
                connEtoC5(e);
            if (e.getSource() == Search.this.getButtonViewEdit()) 
                connEtoC7();
            if (e.getSource() == Search.this.getButtonExport()) 
                connEtoC6(e);
            if (e.getSource() == Search.this.getButtonListAll()) 
                connEtoC8(e);
            if (e.getSource() == Search.this.getButtonCreateDam()) 
                connEtoC9(e);
            if (e.getSource() == Search.this.getButtonPrint()) 
                connEtoC10(e);
            if (e.getSource() == Search.this.getButtonCloneDam()) 
                connEtoC11();
            if (e.getSource() == Search.this.getButtonDeleteADam()) 
                connEtoC12();
            if (e.getSource() == Search.this.getButtonPrerun()) 
                connEtoC13();
        };
    };
    public Search() {
        super();
        initialize();
    }
    /**
     * Insert the method's description here.
     * Creation date: (8/5/2003 9:07:43 AM)
     * @param bHasModel boolean
     */
    public Search(boolean bHasModel) {
        super();
        
        System.out.println("Initializing DAMCAT search tool...");
        
        initialize();
        
        setVisible(true);
    }
    /**
     * Insert the method's description here.
     * Creation date: (7/15/2003 7:45:47 AM)
     */
    public void actionPerformed(ActionEvent e) {
        updateRestrictions();
    }
    /**
     * Insert the method's description here.
     * Creation date: (2/18/2004 6:23:47 PM)
     * @param selection int
     */
    public void clearDeletedSearch(int selection) 
    {
        searchResults.remove(selection);		
        getTableDam().removeAll();
        fillResultTable();		
        getTableDam().repaint();	
    }
    /**
     * connEtoC1:  (ButtonAddRestriction.action.actionPerformed(java.awt.event.ActionEvent) --> DAMCAT_Search.handleRestriction(Z)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC1(java.awt.event.ActionEvent arg1) {
        try {
            // user code begin {1}
            // user code end
            this.handleRestriction(true);
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc) {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }
    /**
     * connEtoC10:  (ButtonPrint.action.actionPerformed(java.awt.event.ActionEvent) --> Search.handlePrintResultTable()V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC10(java.awt.event.ActionEvent arg1) {
        try {
            // user code begin {1}
            // user code end
            this.handlePrintResultTable();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc) {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }
    /**
     * connEtoC11:  (ButtonCloneDam.action. --> Search.handleCloneADam()V)
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC11() {
        try {
            // user code begin {1}
            // user code end
            this.handleCloneADam();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc) {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }
    /**
     * connEtoC12:  (ButtonDeleteADam.action. --> Search.handleDeleteADam()V)
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC12() {
        try {
            // user code begin {1}
            // user code end
            this.handleDeleteADam(new gov.damcat.data.DamInfo());
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc) {
            // user code begin {3}
            System.out.println("Exception in Search.connEtoC12()"); 
            // user code end
            handleException(ivjExc);
        }
    }
    /**
     * connEtoC13:  (ButtonPrerun.action. --> Search.handleDisplayPrerunResults()V)
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC13() {
        try {
            // user code begin {1}
            // user code end
            this.handleDisplayPrerunResults();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc) {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }
    /**
     * connEtoC2:  (ButtonRemoveRestriction.action.actionPerformed(java.awt.event.ActionEvent) --> DAMCAT_Search.handleRestriction(Z)V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC2(java.awt.event.ActionEvent arg1) {
        try {
            // user code begin {1}
            // user code end
            this.handleRestriction(false);
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc) {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }
    /**
     * connEtoC3:  (ButtonClose.action.actionPerformed(java.awt.event.ActionEvent) --> DAMCAT_Search.handleClose()V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC3(java.awt.event.ActionEvent arg1) {
        try {
            // user code begin {1}
            // user code end
            this.handleClose();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc) {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }
    /**
     * connEtoC4:  (ButtonSearch.action.actionPerformed(java.awt.event.ActionEvent) --> DAMCAT_Search.handleSearch()V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC4(java.awt.event.ActionEvent arg1) {
        try {
            // user code begin {1}
            // user code end
            this.handleSearch();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc) {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }
    /**
     * connEtoC5:  (ButtonClear.action.actionPerformed(java.awt.event.ActionEvent) --> DAMCAT_Search.handleClear()V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC5(java.awt.event.ActionEvent arg1) {
        try {
            // user code begin {1}
            // user code end
            this.handleClear();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc) {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }
    /**
     * connEtoC6:  (ButtonExport.action.actionPerformed(java.awt.event.ActionEvent) --> Search.handleExportToSMPDBK()V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC6(java.awt.event.ActionEvent arg1) {
        try {
            // user code begin {1}
            // user code end
            this.handleExportToSMPDBK();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc) {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }
    /**
     * connEtoC7:  (ButtonViewEdit.action. --> DAMCAT_Search.handleViewEdit()V)
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC7() {
        try {
            // user code begin {1}
            // user code end
            this.handleViewEdit();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc) {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }
    /**
     * connEtoC8:  (ButtonListAll.action.actionPerformed(java.awt.event.ActionEvent) --> Search.handleListAllDams()V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC8(java.awt.event.ActionEvent arg1) {
        try {
            // user code begin {1}
            // user code end
            this.handleListAllDams();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc) {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }
    /**
     * connEtoC9:  (ButtonCreateDam.action.actionPerformed(java.awt.event.ActionEvent) --> Search.handleCreateNewDam()V)
     * @param arg1 java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC9(java.awt.event.ActionEvent arg1) {
        try {
            // user code begin {1}
            // user code end
            this.handleCreateNewDam();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc) {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }
    //String inputValue = "";
    //boolean bDone = false;
    
    
    //frame.show();
    
    /*do {
     inputValue = JOptionPane.showInputDialog("New Dam:");
     
     if (inputValue.length() > 10 || inputValue.length() == 0)
     JOptionPane.showMessageDialog(this,"The NID ID must be between 1 and 10 characters.", "Could Not Create Dam", JOptionPane.ERROR_MESSAGE );
     else if (dbAccess.doesDamExist(inputValue)) {	// already exists
     JOptionPane.showMessageDialog(this,"Dam with NID ID \"" + inputValue + "\" already exists.", "Could Not Create Dam", JOptionPane.ERROR_MESSAGE );
     /////////////////
      // NOTE: Check to see if the NIDID already exists and alert the user if it does.
       } else
       bDone = true;
       } while(!bDone);
       
       //////////////
        // NOTE: Create a row for the new NIDID in the damcat_dam table here
         
         newDamInfo.nidid = inputValue;
         
         EditDam edit = new EditDam(newDamInfo,dbAccess);
         edit.show();
         edit.setVisible(true);
         return;*/
    //}
    
    private void fillClonedDam() {
        
        int selection = Integer.parseInt((String)getTableDam().getModel().getValueAt(getTableDam().getSelectedRow(),0)) - 1;
        
        DamInfo selectedDam = (DamInfo)searchResults.get(selection);
        
        DamInfo fullDamInfo = dbAccess.fillCompletely(selectedDam.nidid);
        if (fullDamInfo._nFilled != 2) {
            JOptionPane.showMessageDialog(this,"Error reading data for dam " + selectedDam.nidid + " from DAMCAT database.", "Error", JOptionPane.ERROR_MESSAGE);
        }
        
        // *** to test the new nidid creation where a dam is cloned
        String stateCode = inputState;
        String nextID = dbAccess.findNextID(stateCode);
        System.out.println("Search.fillClonedDam - returned " + nextID + " from " + stateCode);
        
        fullDamInfo.nidid = nextID;
        dbAccess.insertEntireDam(fullDamInfo);
        // *** end test
        
        // *** set EditDam.srchScreen reference to this !
        EditDam edit = new EditDam(fullDamInfo, dbAccess, this);
        //edit.set_searchSelection(selection);
//        edit.show();
        edit.setVisible(true);
        
        return;
    }
    private void fillCreatedDam() {
        
        DamInfo newDamInfo = new DamInfo();
        newDamInfo.nidid = inputState;
        
        // *** to test the new nidid creation where a dam is cloned
        String stateCode = inputState;
        String nextID = dbAccess.findNextID(stateCode);
        System.out.println("Search.fillCreatedDam - returned " + nextID + " from " + stateCode);
        
        newDamInfo.nidid = nextID;
        dbAccess.insertEntireDam(newDamInfo);
        // *** end test
        
        // *** set EditDam.srchScreen reference to this !
        EditDam edit = new EditDam(newDamInfo,dbAccess, this);
        //edit.set_searchSelection(selection);
//        edit.show();
        edit.setVisible(true);
        return;
    }
    /**
     * After a search has been completed, this method fills the JTable
     * with information about the returned dams.
     * Creation date: (7/14/2003 1:26:23 PM)
     */
    private void fillResultTable() {
        // use searchResults to fill the JTable
        UneditableJTableModel tm = new UneditableJTableModel(0,12);
        TableSorter ts = new TableSorter(tm);
        
        for (int i=0; i<searchResults.size(); i++) {
            DamInfo dam = (DamInfo)searchResults.get(i);
            String s[] = { ""+(i+1),dam.dam_name,dam.river,dam.county,dam.nidid };//,dam.other_dam_name,dam.hsa,dam.rfc,dam.downstream_hazard,DamInfo.formatIfPossible(dam.volume_dam),DamInfo.formatIfPossible(dam.latitude_dam),DamInfo.formatIfPossible(dam.longitude_dam) };	
            tm.addRow( (Object[])s);
        }
        
        /*getTableColumn1().setWidth(30);
         getTableColumn2().setWidth(150);
         getTableColumn3().setWidth(150);
         getTableColumn4().setWidth(80);
         getTableColumn5().setWidth(80);
         getTableColumn6().setWidth(80);*/
        
        //	tm.setColumnIdentifiers(new Object[] {"#","Dam Name","River Name","County Name","NIDID","Other Dam Name","HSA","RFC","DH","Volume","Latitude","Longitude"});
        getTableDam().setModel(ts);
        ts.addMouseListenerToHeaderInTable(getTableDam());
        
        if (searchResults.size() == 1)
            ((javax.swing.border.TitledBorder)getPanelResults().getBorder()).setTitle("Search Results - "  + searchResults.size() + " result returned");
        else
            ((javax.swing.border.TitledBorder)getPanelResults().getBorder()).setTitle("Search Results - "  + searchResults.size() + " results returned");
        getPanelResults().repaint();
    }
    /**
     * Return the JButton2 property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getButtonClear() {
        if (ivjButtonClear == null) {
            try {
                ivjButtonClear = new javax.swing.JButton();
                ivjButtonClear.setName("ButtonClear");
                ivjButtonClear.setMnemonic('c');
                ivjButtonClear.setText("Clear Search Fields");
                ivjButtonClear.setBounds(10, 64, 163, 25);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjButtonClear;
    }
    /**
     * Return the ButtonCloneDam property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getButtonCloneDam() {
        if (ivjButtonCloneDam == null) {
            try {
                ivjButtonCloneDam = new javax.swing.JButton();
                ivjButtonCloneDam.setName("ButtonCloneDam");
                ivjButtonCloneDam.setText("Clone A Dam");
                ivjButtonCloneDam.setBounds(10, 121, 164, 25);
                ivjButtonCloneDam.setEnabled(false);
                ivjButtonCloneDam.setActionCommand("Clone A Dam");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjButtonCloneDam;
    }
    /**
     * Return the JButton21 property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getButtonClose() {
        if (ivjButtonClose == null) {
            try {
                ivjButtonClose = new javax.swing.JButton();
                ivjButtonClose.setName("ButtonClose");
                ivjButtonClose.setMnemonic('q');
                ivjButtonClose.setText("Quit");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjButtonClose;
    }
    /**
     * Return the ButtonClear1 property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getButtonCreateDam() {
        if (ivjButtonCreateDam == null) {
            try {
                ivjButtonCreateDam = new javax.swing.JButton();
                ivjButtonCreateDam.setName("ButtonCreateDam");
                ivjButtonCreateDam.setMnemonic('n');
                ivjButtonCreateDam.setText("Create New Dam");
                ivjButtonCreateDam.setBounds(10, 93, 163, 25);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjButtonCreateDam;
    }
    /**
     * Return the ButtonDeleteADam property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getButtonDeleteADam() {
        if (ivjButtonDeleteADam == null) {
            try {
                ivjButtonDeleteADam = new javax.swing.JButton();
                ivjButtonDeleteADam.setName("ButtonDeleteADam");
                ivjButtonDeleteADam.setText("Delete A Dam");
                ivjButtonDeleteADam.setBounds(10, 148, 164, 25);
                ivjButtonDeleteADam.setEnabled(false);
                ivjButtonDeleteADam.setActionCommand("Delete A Dam");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjButtonDeleteADam;
    }
    /**
     * Return the ButtonExport property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getButtonExport() {
        if (ivjButtonExport == null) {
            try {
                ivjButtonExport = new javax.swing.JButton();
                ivjButtonExport.setName("ButtonExport");
                ivjButtonExport.setText("Run SMPDBK Model");
                ivjButtonExport.setMaximumSize(new java.awt.Dimension(160, 25));
                ivjButtonExport.setActionCommand("Run Model");
                ivjButtonExport.setBorderPainted(true);
                ivjButtonExport.setPreferredSize(new java.awt.Dimension(160, 25));
                ivjButtonExport.setEnabled(false);
                ivjButtonExport.setMinimumSize(new java.awt.Dimension(160, 25));
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjButtonExport;
    }
    /**
     * Return the JButton4 property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getButtonFewer() {
        if (ivjButtonFewer == null) {
            try {
                ivjButtonFewer = new javax.swing.JButton();
                ivjButtonFewer.setName("ButtonFewer");
                ivjButtonFewer.setMnemonic('f');
                ivjButtonFewer.setText("Fewer");
                ivjButtonFewer.setMaximumSize(new java.awt.Dimension(72, 25));
                ivjButtonFewer.setPreferredSize(new java.awt.Dimension(72, 25));
                ivjButtonFewer.setBounds(403, 135, 77, 20);
                ivjButtonFewer.setMinimumSize(new java.awt.Dimension(72, 25));
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjButtonFewer;
    }
    
    /**
     * Return the ButtonListAll property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getButtonListAll() {
        if (ivjButtonListAll == null) {
            try {
                ivjButtonListAll = new javax.swing.JButton();
                ivjButtonListAll.setName("ButtonListAll");
                ivjButtonListAll.setMnemonic('A');
                ivjButtonListAll.setText("List All Dams");
                ivjButtonListAll.setBounds(10, 36, 163, 25);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjButtonListAll;
    }
    /**
     * Return the JButton3 property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getButtonMore() {
        if (ivjButtonMore == null) {
            try {
                ivjButtonMore = new javax.swing.JButton();
                ivjButtonMore.setName("ButtonMore");
                ivjButtonMore.setMnemonic('m');
                ivjButtonMore.setText("More");
                ivjButtonMore.setMaximumSize(new java.awt.Dimension(68, 25));
                ivjButtonMore.setPreferredSize(new java.awt.Dimension(68, 25));
                ivjButtonMore.setBounds(315, 135, 75, 20);
                ivjButtonMore.setMinimumSize(new java.awt.Dimension(68, 25));
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjButtonMore;
    }
    
    /**
     * Return the ButtonPrerun property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getButtonPrerun() {
        if (ivjButtonPrerun == null) {
            try {
                ivjButtonPrerun = new javax.swing.JButton();
                ivjButtonPrerun.setName("ButtonPrerun");
                ivjButtonPrerun.setText("View Forecast Info");
                ivjButtonPrerun.setMaximumSize(new java.awt.Dimension(160, 25));
                ivjButtonPrerun.setActionCommand("Prerun Results");
                ivjButtonPrerun.setPreferredSize(new java.awt.Dimension(160, 25));
                ivjButtonPrerun.setEnabled(false);
                ivjButtonPrerun.setMinimumSize(new java.awt.Dimension(160, 25));
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjButtonPrerun;
    }
    /**
     * Return the ButtonPrint property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getButtonPrint() {
        if (ivjButtonPrint == null) {
            try {
                ivjButtonPrint = new javax.swing.JButton();
                ivjButtonPrint.setName("ButtonPrint");
                ivjButtonPrint.setMnemonic('P');
                ivjButtonPrint.setText("Print");
                ivjButtonPrint.setActionCommand("Print Result Table");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjButtonPrint;
    }
    /**
     * Return the JButton1 property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getButtonSearch() {
        if (ivjButtonSearch == null) {
            try {
                ivjButtonSearch = new javax.swing.JButton();
                ivjButtonSearch.setName("ButtonSearch");
                ivjButtonSearch.setMnemonic('S');
                ivjButtonSearch.setText("Search");
                ivjButtonSearch.setBounds(10, 8, 163, 25);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjButtonSearch;
    }
    /**
     * Return the JButton311 property value.
     * @return javax.swing.JButton
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JButton getButtonViewEdit() {
        if (ivjButtonViewEdit == null) {
            try {
                ivjButtonViewEdit = new javax.swing.JButton();
                ivjButtonViewEdit.setName("ButtonViewEdit");
                ivjButtonViewEdit.setText("Edit Dam Data");
                ivjButtonViewEdit.setMaximumSize(new java.awt.Dimension(150, 25));
                ivjButtonViewEdit.setBorderPainted(true);
                ivjButtonViewEdit.setPreferredSize(new java.awt.Dimension(150, 25));
                ivjButtonViewEdit.setEnabled(false);
                ivjButtonViewEdit.setMinimumSize(new java.awt.Dimension(150, 25));
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjButtonViewEdit;
    }
    /**
     * Return the ComboOperator property value.
     * @return javax.swing.JComboBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JComboBox getComboOperator1() {
        if (ivjComboOperator1 == null) {
            try {
                ivjComboOperator1 = new javax.swing.JComboBox();
                ivjComboOperator1.setName("ComboOperator1");
                ivjComboOperator1.setBounds(169, 29, 98, 23);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjComboOperator1;
    }
    /**
     * Return the ComboOperator2 property value.
     * @return javax.swing.JComboBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JComboBox getComboOperator2() {
        if (ivjComboOperator2 == null) {
            try {
                ivjComboOperator2 = new javax.swing.JComboBox();
                ivjComboOperator2.setName("ComboOperator2");
                ivjComboOperator2.setBounds(169, 59, 98, 23);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjComboOperator2;
    }
    /**
     * Return the ComboOperator3 property value.
     * @return javax.swing.JComboBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JComboBox getComboOperator3() {
        if (ivjComboOperator3 == null) {
            try {
                ivjComboOperator3 = new javax.swing.JComboBox();
                ivjComboOperator3.setName("ComboOperator3");
                ivjComboOperator3.setBounds(169, 88, 98, 23);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjComboOperator3;
    }
    /**
     * Return the JComboBox1 property value.
     * @return javax.swing.JComboBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JComboBox getComboRestriction1() {
        if (ivjComboRestriction1 == null) {
            try {
                ivjComboRestriction1 = new javax.swing.JComboBox();
                ivjComboRestriction1.setName("ComboRestriction1");
                ivjComboRestriction1.setBounds(30, 28, 111, 23);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjComboRestriction1;
    }
    /**
     * Return the JComboBox11 property value.
     * @return javax.swing.JComboBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JComboBox getComboRestriction2() {
        if (ivjComboRestriction2 == null) {
            try {
                ivjComboRestriction2 = new javax.swing.JComboBox();
                ivjComboRestriction2.setName("ComboRestriction2");
                ivjComboRestriction2.setBounds(30, 58, 112, 23);
                ivjComboRestriction2.setVisible(true);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjComboRestriction2;
    }
    /**
     * Return the JComboBox12 property value.
     * @return javax.swing.JComboBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JComboBox getComboRestriction3() {
        if (ivjComboRestriction3 == null) {
            try {
                ivjComboRestriction3 = new javax.swing.JComboBox();
                ivjComboRestriction3.setName("ComboRestriction3");
                ivjComboRestriction3.setBounds(30, 87, 112, 23);
                ivjComboRestriction3.setVisible(true);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjComboRestriction3;
    }
    /**
     * Return the JFrameContentPane property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getJFrameContentPane() {
        if (ivjJFrameContentPane == null) {
            try {
                ivjJFrameContentPane = new javax.swing.JPanel();
                ivjJFrameContentPane.setName("JFrameContentPane");
                ivjJFrameContentPane.setLayout(new java.awt.BorderLayout());
                getJFrameContentPane().add(getPanelTop(), "North");
                getJFrameContentPane().add(getPanelResults(), "Center");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjJFrameContentPane;
    }
    /**
     * Return the LabelBetween1 property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getLabelBetween1() {
        if (ivjLabelBetween1 == null) {
            try {
                ivjLabelBetween1 = new javax.swing.JLabel();
                ivjLabelBetween1.setName("LabelBetween1");
                ivjLabelBetween1.setText("and");
                ivjLabelBetween1.setBounds(390, 33, 26, 14);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjLabelBetween1;
    }
    /**
     * Return the LabelBetween2 property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getLabelBetween2() {
        if (ivjLabelBetween2 == null) {
            try {
                ivjLabelBetween2 = new javax.swing.JLabel();
                ivjLabelBetween2.setName("LabelBetween2");
                ivjLabelBetween2.setText("and");
                ivjLabelBetween2.setBounds(388, 63, 26, 14);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjLabelBetween2;
    }
    /**
     * Return the LabelBetween3 property value.
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getLabelBetween3() {
        if (ivjLabelBetween3 == null) {
            try {
                ivjLabelBetween3 = new javax.swing.JLabel();
                ivjLabelBetween3.setName("LabelBetween3");
                ivjLabelBetween3.setText("and");
                ivjLabelBetween3.setBounds(388, 91, 26, 14);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjLabelBetween3;
    }
    /**
     * Return the PanelDamButtons property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getPanelDamButtons() {
        if (ivjPanelDamButtons == null) {
            try {
                ivjPanelDamButtons = new javax.swing.JPanel();
                ivjPanelDamButtons.setName("PanelDamButtons");
                ivjPanelDamButtons.setPreferredSize(new java.awt.Dimension(10, 35));
                ivjPanelDamButtons.setLayout(getPanelDamButtonsFlowLayout());
                ivjPanelDamButtons.setBackground(new java.awt.Color(204,204,204));
                getPanelDamButtons().add(getButtonViewEdit(), getButtonViewEdit().getName());
                getPanelDamButtons().add(getButtonPrerun(), getButtonPrerun().getName());
                getPanelDamButtons().add(getButtonExport(), getButtonExport().getName());
                ivjPanelDamButtons.add(getButtonPrint());
                ivjPanelDamButtons.add(getButtonClose());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjPanelDamButtons;
    }
    /**
     * Return the PanelDamButtonsFlowLayout property value.
     * @return java.awt.FlowLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private java.awt.FlowLayout getPanelDamButtonsFlowLayout() {
        java.awt.FlowLayout ivjPanelDamButtonsFlowLayout = null;
        try {
            /* Create part */
            ivjPanelDamButtonsFlowLayout = new java.awt.FlowLayout();
            ivjPanelDamButtonsFlowLayout.setHgap(25);
        } catch (java.lang.Throwable ivjExc) {
            handleException(ivjExc);
        };
        return ivjPanelDamButtonsFlowLayout;
    }
    /**
     * Return the JPanel3 property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getPanelRestrictions() {
        if (ivjPanelRestrictions == null) {
            try {
                ivjPanelRestrictions = new javax.swing.JPanel();
                ivjPanelRestrictions.setName("PanelRestrictions");
                ivjPanelRestrictions.setPreferredSize(new java.awt.Dimension(416, 150));
                ivjPanelRestrictions.setBorder(new javax.swing.border.TitledBorder("Search Options"));
                ivjPanelRestrictions.setLayout(null);
                ivjPanelRestrictions.setBackground(new java.awt.Color(204,204,204));
                getPanelRestrictions().add(getTextRestriction1(), getTextRestriction1().getName());
                getPanelRestrictions().add(getComboRestriction1(), getComboRestriction1().getName());
                getPanelRestrictions().add(getComboOperator1(), getComboOperator1().getName());
                getPanelRestrictions().add(getTextRestrictionBetween1_1(), getTextRestrictionBetween1_1().getName());
                getPanelRestrictions().add(getLabelBetween1(), getLabelBetween1().getName());
                getPanelRestrictions().add(getTextRestrictionBetween1_2(), getTextRestrictionBetween1_2().getName());
                getPanelRestrictions().add(getComboRestriction2(), getComboRestriction2().getName());
                getPanelRestrictions().add(getComboOperator2(), getComboOperator2().getName());
                getPanelRestrictions().add(getTextRestriction2(), getTextRestriction2().getName());
                getPanelRestrictions().add(getTextRestrictionBetween2_1(), getTextRestrictionBetween2_1().getName());
                getPanelRestrictions().add(getLabelBetween2(), getLabelBetween2().getName());
                getPanelRestrictions().add(getTextRestrictionBetween2_2(), getTextRestrictionBetween2_2().getName());
                getPanelRestrictions().add(getComboRestriction3(), getComboRestriction3().getName());
                getPanelRestrictions().add(getComboOperator3(), getComboOperator3().getName());
                getPanelRestrictions().add(getTextRestriction3(), getTextRestriction3().getName());
                getPanelRestrictions().add(getTextRestrictionBetween3_1(), getTextRestrictionBetween3_1().getName());
                getPanelRestrictions().add(getLabelBetween3(), getLabelBetween3().getName());
                getPanelRestrictions().add(getTextRestrictionBetween3_2(), getTextRestrictionBetween3_2().getName());
                getPanelRestrictions().add(getButtonMore(), getButtonMore().getName());
                getPanelRestrictions().add(getButtonFewer(), getButtonFewer().getName());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjPanelRestrictions;
    }
    /**
     * Return the JPanel2 property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getPanelResults() {
        if (ivjPanelResults == null) {
            try {
                ivjPanelResults = new javax.swing.JPanel();
                ivjPanelResults.setName("PanelResults");
                ivjPanelResults.setBorder(new javax.swing.border.TitledBorder("Search Results"));
                ivjPanelResults.setLayout(getPanelResultsBorderLayout());
                getPanelResults().add(getScrollDam(), "Center");
                getPanelResults().add(getPanelDamButtons(), "South");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjPanelResults;
    }
    /**
     * Return the PanelResultsBorderLayout property value.
     * @return java.awt.BorderLayout
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private java.awt.BorderLayout getPanelResultsBorderLayout() {
        java.awt.BorderLayout ivjPanelResultsBorderLayout = null;
        try {
            /* Create part */
            ivjPanelResultsBorderLayout = new java.awt.BorderLayout();
            ivjPanelResultsBorderLayout.setVgap(5);
            ivjPanelResultsBorderLayout.setHgap(0);
        } catch (java.lang.Throwable ivjExc) {
            handleException(ivjExc);
        };
        return ivjPanelResultsBorderLayout;
    }
    /**
     * Return the PanelSearchButtons property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getPanelSearchButtons() {
        if (ivjPanelSearchButtons == null) {
            try {
                ivjPanelSearchButtons = new javax.swing.JPanel();
                ivjPanelSearchButtons.setName("PanelSearchButtons");
                ivjPanelSearchButtons.setPreferredSize(new java.awt.Dimension(180, 150));
                ivjPanelSearchButtons.setLayout(null);
                ivjPanelSearchButtons.setBackground(new java.awt.Color(204,204,204));
                getPanelSearchButtons().add(getButtonClear(), getButtonClear().getName());
                getPanelSearchButtons().add(getButtonSearch(), getButtonSearch().getName());
                getPanelSearchButtons().add(getButtonListAll(), getButtonListAll().getName());
                getPanelSearchButtons().add(getButtonCreateDam(), getButtonCreateDam().getName());
                getPanelSearchButtons().add(getButtonCloneDam(), getButtonCloneDam().getName());
                getPanelSearchButtons().add(getButtonDeleteADam(), getButtonDeleteADam().getName());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjPanelSearchButtons;
    }
    /**
     * Return the PanelTop property value.
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getPanelTop() {
        if (ivjPanelTop == null) {
            try {
                ivjPanelTop = new javax.swing.JPanel();
                ivjPanelTop.setName("PanelTop");
                ivjPanelTop.setPreferredSize(new java.awt.Dimension(605, 175));
                ivjPanelTop.setLayout(new java.awt.BorderLayout());
                getPanelTop().add(getPanelRestrictions(), "Center");
                getPanelTop().add(getPanelSearchButtons(), "East");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjPanelTop;
    }
    /**
     * Return the JScrollPane1 property value.
     * @return javax.swing.JScrollPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JScrollPane getScrollDam() {
        if (ivjScrollDam == null) {
            try {
                ivjScrollDam = new javax.swing.JScrollPane();
                ivjScrollDam.setName("ScrollDam");
                ivjScrollDam.setVerticalScrollBarPolicy(javax.swing.JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
                ivjScrollDam.setHorizontalScrollBarPolicy(javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
                ivjScrollDam.setBackground(java.awt.SystemColor.control);
                getScrollDam().setViewportView(getTableDam());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjScrollDam;
    }
    /**
     * Return the TableColumn1 property value.
     * @return javax.swing.table.TableColumn
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.table.TableColumn getTableColumn1() {
        if (ivjTableColumn1 == null) {
            try {
                ivjTableColumn1 = new javax.swing.table.TableColumn();
                ivjTableColumn1.setIdentifier("RowNum");
                ivjTableColumn1.setWidth(25);
                ivjTableColumn1.setHeaderValue("#");
                ivjTableColumn1.setMinWidth(25);
                ivjTableColumn1.setMaxWidth(50);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjTableColumn1;
    }
    /**
     * Return the TableColumn2 property value.
     * @return javax.swing.table.TableColumn
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.table.TableColumn getTableColumn2() {
        if (ivjTableColumn2 == null) {
            try {
                ivjTableColumn2 = new javax.swing.table.TableColumn();
                ivjTableColumn2.setModelIndex(1);
                ivjTableColumn2.setWidth(170);
                ivjTableColumn2.setHeaderValue("Dam Name");
                ivjTableColumn2.setMinWidth(170);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjTableColumn2;
    }
    /**
     * Return the TableColumn3 property value.
     * @return javax.swing.table.TableColumn
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.table.TableColumn getTableColumn3() {
        if (ivjTableColumn3 == null) {
            try {
                ivjTableColumn3 = new javax.swing.table.TableColumn();
                ivjTableColumn3.setModelIndex(2);
                ivjTableColumn3.setWidth(170);
                ivjTableColumn3.setHeaderValue("River Name");
                ivjTableColumn3.setMinWidth(170);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjTableColumn3;
    }
    /**
     * Return the TableColumn4 property value.
     * @return javax.swing.table.TableColumn
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.table.TableColumn getTableColumn4() {
        if (ivjTableColumn4 == null) {
            try {
                ivjTableColumn4 = new javax.swing.table.TableColumn();
                ivjTableColumn4.setModelIndex(3);
                ivjTableColumn4.setWidth(100);
                ivjTableColumn4.setHeaderValue("County Name");
                ivjTableColumn4.setMinWidth(100);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjTableColumn4;
    }
    /**
     * Return the TableColumn5 property value.
     * @return javax.swing.table.TableColumn
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.table.TableColumn getTableColumn5() {
        if (ivjTableColumn5 == null) {
            try {
                ivjTableColumn5 = new javax.swing.table.TableColumn();
                ivjTableColumn5.setModelIndex(4);
                ivjTableColumn5.setWidth(70);
                ivjTableColumn5.setHeaderValue("NIDID");
                ivjTableColumn5.setMinWidth(70);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjTableColumn5;
    }
    /**
     * Return the ScrollPaneTable property value.
     * @return javax.swing.JTable
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTable getTableDam() {
        if (ivjTableDam == null) {
            try {
                ivjTableDam = new javax.swing.JTable();
                ivjTableDam.setName("TableDam");
                getScrollDam().setColumnHeaderView(ivjTableDam.getTableHeader());
                getScrollDam().getViewport().setBackingStoreEnabled(true);
                ivjTableDam.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_ALL_COLUMNS);
                ivjTableDam.setBounds(0, 0, 200, 200);
                ivjTableDam.setAutoCreateColumnsFromModel(false);
                ivjTableDam.addColumn(getTableColumn1());
                ivjTableDam.addColumn(getTableColumn2());
                ivjTableDam.addColumn(getTableColumn3());
                ivjTableDam.addColumn(getTableColumn4());
                ivjTableDam.addColumn(getTableColumn5());
                // user code begin {1}
                ivjTableDam.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                ivjTableDam.getSelectionModel().addListSelectionListener(this);
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjTableDam;
    }
    /**
     * Return the JTextField1 property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField getTextRestriction1() {
        if (ivjTextRestriction1 == null) {
            try {
                ivjTextRestriction1 = new javax.swing.JTextField();
                ivjTextRestriction1.setName("TextRestriction1");
                ivjTextRestriction1.setBounds(309, 30, 180, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjTextRestriction1;
    }
    /**
     * Return the JTextField11 property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField getTextRestriction2() {
        if (ivjTextRestriction2 == null) {
            try {
                ivjTextRestriction2 = new javax.swing.JTextField();
                ivjTextRestriction2.setName("TextRestriction2");
                ivjTextRestriction2.setBounds(309, 60, 180, 20);
                ivjTextRestriction2.setVisible(true);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjTextRestriction2;
    }
    /**
     * Return the JTextField12 property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField getTextRestriction3() {
        if (ivjTextRestriction3 == null) {
            try {
                ivjTextRestriction3 = new javax.swing.JTextField();
                ivjTextRestriction3.setName("TextRestriction3");
                ivjTextRestriction3.setBounds(309, 88, 180, 20);
                ivjTextRestriction3.setVisible(true);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjTextRestriction3;
    }
    /**
     * Return the TextRestrictionBetween1_1 property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField getTextRestrictionBetween1_1() {
        if (ivjTextRestrictionBetween1_1 == null) {
            try {
                ivjTextRestrictionBetween1_1 = new javax.swing.JTextField();
                ivjTextRestrictionBetween1_1.setName("TextRestrictionBetween1_1");
                ivjTextRestrictionBetween1_1.setBounds(310, 30, 75, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjTextRestrictionBetween1_1;
    }
    /**
     * Return the TextRestrictionBetween1_2 property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField getTextRestrictionBetween1_2() {
        if (ivjTextRestrictionBetween1_2 == null) {
            try {
                ivjTextRestrictionBetween1_2 = new javax.swing.JTextField();
                ivjTextRestrictionBetween1_2.setName("TextRestrictionBetween1_2");
                ivjTextRestrictionBetween1_2.setBounds(416, 30, 75, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjTextRestrictionBetween1_2;
    }
    /**
     * Return the TextRestrictionBetween2_1 property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField getTextRestrictionBetween2_1() {
        if (ivjTextRestrictionBetween2_1 == null) {
            try {
                ivjTextRestrictionBetween2_1 = new javax.swing.JTextField();
                ivjTextRestrictionBetween2_1.setName("TextRestrictionBetween2_1");
                ivjTextRestrictionBetween2_1.setBounds(308, 60, 75, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjTextRestrictionBetween2_1;
    }
    /**
     * Return the TextRestrictionBetween2_2 property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField getTextRestrictionBetween2_2() {
        if (ivjTextRestrictionBetween2_2 == null) {
            try {
                ivjTextRestrictionBetween2_2 = new javax.swing.JTextField();
                ivjTextRestrictionBetween2_2.setName("TextRestrictionBetween2_2");
                ivjTextRestrictionBetween2_2.setBounds(414, 60, 75, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjTextRestrictionBetween2_2;
    }
    /**
     * Return the TextRestrictionBetween3_1 property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField getTextRestrictionBetween3_1() {
        if (ivjTextRestrictionBetween3_1 == null) {
            try {
                ivjTextRestrictionBetween3_1 = new javax.swing.JTextField();
                ivjTextRestrictionBetween3_1.setName("TextRestrictionBetween3_1");
                ivjTextRestrictionBetween3_1.setBounds(308, 88, 75, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjTextRestrictionBetween3_1;
    }
    /**
     * Return the TextRestrictionBetween3_2 property value.
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField getTextRestrictionBetween3_2() {
        if (ivjTextRestrictionBetween3_2 == null) {
            try {
                ivjTextRestrictionBetween3_2 = new javax.swing.JTextField();
                ivjTextRestrictionBetween3_2.setName("TextRestrictionBetween3_2");
                ivjTextRestrictionBetween3_2.setBounds(414, 88, 75, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc) {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjTextRestrictionBetween3_2;
    }
    /**
     * Handle "Clear" button clicked.
     */
    private void handleClear() {
        getTextRestriction1().setText("");
        getTextRestriction2().setText("");
        getTextRestriction3().setText("");
        getTextRestrictionBetween1_1().setText("");
        getTextRestrictionBetween1_2().setText("");
        getTextRestrictionBetween2_1().setText("");
        getTextRestrictionBetween2_2().setText("");
        getTextRestrictionBetween3_1().setText("");
        getTextRestrictionBetween3_2().setText("");
        getTableDam().setModel(new UneditableJTableModel(0,6));
        ((javax.swing.border.TitledBorder)getPanelResults().getBorder()).setTitle("Search Results");
        getPanelResults().repaint();
        searchResults = null;
        return;
    }
    /**
     * Comment
     */
    private void handleCloneADam() {
        
        buttonCloneDam = true;
        
        JFrame frame = new ComboBoxFrame();
        return;
    }
    /**
     * Called when the "Close" button is clicked.
     */
    private void handleClose() {
        if (hasSpawned) {
            int confirmOption;
            confirmOption = JOptionPane.showConfirmDialog(this, "This will close all Dambreak Analysis windows. Are you sure you want to quit?", "Confirm", JOptionPane.YES_NO_OPTION);
            
            if (confirmOption == JOptionPane.NO_OPTION)
                return;
        }
        
        dispose();
        System.exit(0);
        
        return;
    }
    /**
     * Comment
     */
    private void handleCreateNewDam() {
        
        JFrame frame = new ComboBoxFrame();
        return;
    }
    private void handleDeleteADam(DamInfo damInfo) {
        
        // *** The DamInfo passed in is empty !!!
        
        // *** So, we need to replace it with the one selected !
        
        int selection = Integer.parseInt((String)getTableDam().getModel().getValueAt(getTableDam().getSelectedRow(),0)) - 1;
        // System.out.println("Delete selection # " + selection);
        
        damInfo = (DamInfo)searchResults.get(selection);
        
        String buttons [] = {"OK", "Cancel"};
        String damID = damInfo.getNidid().trim();
        int confirmOption;
        confirmOption = JOptionPane.showOptionDialog(null, "Do you want to delete the dam\n" + damID + " from the database?", "Confirm Delete", JOptionPane.OK_CANCEL_OPTION,JOptionPane.QUESTION_MESSAGE,
                null,buttons, buttons[1]);
        if (confirmOption == 0)
        {
            JOptionPane.showMessageDialog(null,"The dam " + damID + " was deleted");
            dbAccess.deleteSelectedDam(damID);
            getButtonDeleteADam().setEnabled(false);
            
            // *** Now remove selection from tables and repaint
            searchResults.remove(selection);		
            getTableDam().removeAll();
            fillResultTable();		
            getTableDam().repaint();			
            
        }
        else if (confirmOption == 1)
            return;
        
        
        return;
    }
    /**
     * Insert the method's description here.
     * Creation date: (2/19/2004 2:32:48 PM)
     */
    public void handleDisplayPrerunResults() {
        
        // make sure a row is selected
        if (getTableDam().getSelectedRow() == -1)
            return;
        
        int selection = Integer.parseInt((String)getTableDam().getModel().getValueAt(getTableDam().getSelectedRow(),0)) - 1;
        
        // get the data for that row
        DamInfo damInfo = (DamInfo)searchResults.get(selection);
        
        damInfo = dbAccess.fillCompletely(damInfo.nidid);
        
        AnalysisData currentData = damInfo.getAnalysisData();
        
        String oError = currentData.verifyOutput();
        
        boolean bEmptyDataset = currentData. bEmptyDataset; 
        //System.out.println("bEmptyDataset: " + bEmptyDataset);
        if (bEmptyDataset)
        {
            JOptionPane.showMessageDialog(null,"The dam with NIDID = " + damInfo.nidid + " has empty dataset." + 
                    "\nMinimum one scenario must be added to create prerun results.", "Error", JOptionPane.ERROR_MESSAGE);
            return;
        }
        
        if(!oError.equalsIgnoreCase(""))
        {
            JOptionPane.showMessageDialog(this,oError,"Could Not Display Stored Forecast",JOptionPane.ERROR_MESSAGE);
            handleExportToSMPDBK();
            return;
        }
        else
        {
            OutputManager outMan = new OutputManager(this, currentData);
        }
        
    }
    /**
     * Called whenever the part throws an exception.
     * @param exception java.lang.Throwable
     */
    private void handleException(java.lang.Throwable exception) {
        
        /*Uncomment the following lines to print uncaught exceptions to stdout */
        System.out.println("--------- UNCAUGHT EXCEPTION ---------");
        exception.printStackTrace(System.out);
    }
    /**
     * Called when "Export to SMPDBK" is clicked.
     */
    private void handleExportToSMPDBK() {
        
        // make sure a row is selected
        if (getTableDam().getSelectedRow() == -1)
            return;
        
        int selection = Integer.parseInt((String)getTableDam().getModel().getValueAt(getTableDam().getSelectedRow(),0)) - 1;
        
        // get the data for that row
        DamInfo damInfo = (DamInfo)searchResults.get(selection);
        
        damInfo = dbAccess.fillCompletely(damInfo.nidid);
        
        gov.dambreak.smpdbk.ModelGUI smpdbkInputEditor = new gov.dambreak.smpdbk.ModelGUI(true,damInfo.nidid,damInfo.getAnalysisData(),dbAccess);
        
        // smpdbkInputEditor.getmenuRun_SMPDBK().fireMenuKeyPressed(new MenuKeyEvent());
        // MenuSelectionManager myManager = MenuSelectionManager.defaultManager();
        // myManager.setSelectedPath(smpdbkInputEditor.getmenuRun_SMPDBK().getSubElements());
        // smpdbkInputEditor.getmenuRun_SMPDBK().menuSelectionChanged(true);
        // long timeNow = System.currentTimeMillis(); 
        // MouseEvent myEvent = new MouseEvent(smpdbkInputEditor.getmenuRun_SMPDBK(), 100000, timeNow, 0, 0, 0, 1, false);
        // smpdbkInputEditor.getmenuRun_SMPDBK().processMouseEvent(myEvent, smpdbkInputEditor.getmenuRun_SMPDBK().getSubElements(), myManager); 
        
        smpdbkInputEditor.handleMenuRun_SMPDBK();
        
        hasSpawned = true;
    }
    /**
     * Insert the method's description here.
     * Creation date: (2/19/2004 2:32:48 PM)
     */
    public void handleHolder() {
        
        
        // make sure a row is selected
        if (getTableDam().getSelectedRow() == -1)
            return;
        
        int selection = Integer.parseInt((String)getTableDam().getModel().getValueAt(getTableDam().getSelectedRow(),0)) - 1;
        
        // get the data for that row
        DamInfo damInfo = (DamInfo)searchResults.get(selection);
        
        damInfo = dbAccess.fillCompletely(damInfo.nidid);
        
        AnalysisData currentData = damInfo.getAnalysisData();
        
        // *** make sure textAvailable flag is NOT set !
        
        OutputManager outMan = new OutputManager(this, currentData);
        
        // hasSpawned = true;  // *** ?????
        
    }
    /**
     * Comment
     */
    public void handleListAllDams() {
        // change to hourglass cursor
        setCursor(java.awt.Cursor.getPredefinedCursor(java.awt.Cursor.WAIT_CURSOR));
        
        searchResults = dbAccess.search(null,null,null,null);
        fillResultTable();
        
        // switch back to arrow cursor
        setCursor(null);
    }
    /**
     * This method is called when "Print Result Table" is clicked.
     */
    private void handlePrintResultTable() {
        TablePrinter tp = new TablePrinter(getTableDam());
        return;
    }
    /**
     * Called when "Add Restriction" or "Remove Restriction"
     * buttons are clicked.
     */
    private void handleRestriction(boolean bAdd) {
        if (bAdd)
            numRestrictions++;
        else
            numRestrictions--;
        
        updateRestrictions();
    }
    /**
     * Called when "Search" is clicked.
     */
    private void handleSearch() {
        // change to hourglass cursor
        setCursor(java.awt.Cursor.getPredefinedCursor(java.awt.Cursor.WAIT_CURSOR));
        
        String	field[] = new String[3], oper[] = new String[3], data1[] = new String[3], data2[] = new String[3];
        if (numRestrictions >= 1) {
            field[0] = (String)getComboRestriction1().getSelectedItem();
            oper[0] = (String)getComboOperator1().getSelectedItem();
            if (oper[0].equals("is between")) {
                data1[0] = getTextRestrictionBetween1_1().getText().toUpperCase();
                data2[0] = getTextRestrictionBetween1_2().getText().toUpperCase();
            } 
            else
            {
                data1[0] = getTextRestriction1().getText().toUpperCase();
            }
        }
        if (numRestrictions >= 2) {
            field[1] = (String)getComboRestriction2().getSelectedItem();
            oper[1] = (String)getComboOperator2().getSelectedItem();
            if (oper[1].equals("is between")) {
                data1[1] = getTextRestrictionBetween2_1().getText().toUpperCase();
                data2[1] = getTextRestrictionBetween2_2().getText().toUpperCase();
            } 
            else
            {
                data1[1] = getTextRestriction2().getText().toUpperCase();
            }
        }
        if (numRestrictions >= 3) {
            field[2] = (String)getComboRestriction3().getSelectedItem();
            oper[2] = (String)getComboOperator3().getSelectedItem();
            if (oper[2].equals("is between")) {
                data1[2] = getTextRestrictionBetween3_1().getText().toUpperCase();
                data2[2] = getTextRestrictionBetween3_2().getText().toUpperCase();
            }
            else
            {
                data1[2] = getTextRestriction3().getText().toUpperCase();
            }
        }
        
        try {
            searchResults = dbAccess.search(field,oper,data1,data2);
            
            Collections.sort(searchResults, new Comparator() {
                public int compare(Object a, Object b) {
                    DamInfo damInfo_a = (DamInfo)a;
                    DamInfo damInfo_b = (DamInfo)b;
                    return damInfo_a.getDam_name().compareToIgnoreCase(damInfo_b.getDam_name());
                }    
            });
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(this,"Error searching DAMCAT.  Invalid parameters.", "Error",JOptionPane.ERROR_MESSAGE);
        }
        fillResultTable();
        
        // switch back to arrow cursor
        setCursor(null);
        return;
    }
    /**
     * Handle clicks to "View / Edit Dam Data" button.
     */
    private void handleViewEdit() {
        
        int selection = Integer.parseInt((String)getTableDam().getModel().getValueAt(getTableDam().getSelectedRow(),0)) - 1;
        
        DamInfo selectedDam = (DamInfo)searchResults.get(selection);
        
        DamInfo fullDamInfo = dbAccess.fillCompletely(selectedDam.nidid);
        if (fullDamInfo._nFilled != 2) {
            JOptionPane.showMessageDialog(this,"Error reading data for dam " + selectedDam.nidid + " from DAMCAT database.", "Error", JOptionPane.ERROR_MESSAGE);
        }
        
        EditDam edit = new EditDam(fullDamInfo, dbAccess, this);
        edit.set_searchSelection(selection);
//        edit.show();
        edit.setVisible(true);
        return;
    }
    /**
     * Initializes connections
     * @exception java.lang.Exception The exception description.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initConnections() throws java.lang.Exception {
        // user code begin {1}
        // user code end
        getButtonMore().addActionListener(ivjEventHandler);
        getButtonFewer().addActionListener(ivjEventHandler);
        getButtonClose().addActionListener(ivjEventHandler);
        getButtonSearch().addActionListener(ivjEventHandler);
        getButtonClear().addActionListener(ivjEventHandler);
        getButtonViewEdit().addActionListener(ivjEventHandler);
        getButtonExport().addActionListener(ivjEventHandler);
        getButtonListAll().addActionListener(ivjEventHandler);
        getButtonCreateDam().addActionListener(ivjEventHandler);
        getButtonPrint().addActionListener(ivjEventHandler);
        getButtonCloneDam().addActionListener(ivjEventHandler);
        getButtonDeleteADam().addActionListener(ivjEventHandler);
        getButtonPrerun().addActionListener(ivjEventHandler);
    }
    /**
     * Initialize the class.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initialize() {
        try {
            // user code begin {1}
            // user code end
            setName("DAMCAT_Search");
            setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
            setResizable(true);
            setSize(745, 523);
            setTitle("DAMCAT - Search Tool");
            setContentPane(getJFrameContentPane());
            initConnections();
        } catch (java.lang.Throwable ivjExc) {
            handleException(ivjExc);
        }
        // user code begin {2}
        
        /**
         * Initialize restriction combo boxes.
         */
        
        /* ShapeLayer shapeLayer = new ShapeLayer();
         Properties shapeLayerProps = new Properties();
         shapeLayerProps.put("prettyName", "Political Solid");
         shapeLayerProps.put("lineColor", "000000");
         shapeLayerProps.put("fillColor", "BDDE83");
         shapeLayerProps.put("shapeFile", "D:/openMap/openmap-4.5.4/share/data/shape/dcwpo-browse.shp");
         shapeLayerProps.put("spatialIndex", "D:/openMap/openmap-4.5.4/share/data/shape/dcwpo-browse.ssx");
         shapeLayer.setProperties(shapeLayerProps);
         getmapBean().add(shapeLayer);
         
         LocationLayer loc = new LocationLayer();
         loc.setLocationHandlers( new LocationHandler[] { new BasicLocationHandler() } );
         getmapBean().add(loc);*/
        //DamMapper map = new DamMapper("Dam Map");
        
        // store all three in an array for easy access
        JComboBox restrictions[] = new JComboBox[3];
        restrictions[0] = getComboRestriction1(); restrictions[1] = getComboRestriction2(); restrictions[2] = getComboRestriction3();
        
        // add key listeners to the restriction boxes to handle "Enter" presses
        ivjPanelTop.getRootPane().setDefaultButton(ivjButtonSearch);
        ivjTextRestriction1.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == e.VK_ENTER)
                    ivjButtonSearch.doClick();
            }
        });
        ivjTextRestriction2.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == e.VK_ENTER)
                    ivjButtonSearch.doClick();
            }
        });
        ivjTextRestriction3.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == e.VK_ENTER)
                    ivjButtonSearch.doClick();
            }
        });
        
        // initialize the items in the restriction boxes
        for (int i=0; i<3; i++) {
            restrictions[i].addItem("Dam Name");
            restrictions[i].addItem("River Name");
            restrictions[i].addItem("County Name");
            restrictions[i].addItem("NIDID");
            restrictions[i].addItem("HSA");
            restrictions[i].addItem("RFC");
            restrictions[i].addItem("DH");
            restrictions[i].addItem("Max Storage");
            restrictions[i].addItem("Longitude");
            restrictions[i].addItem("Latitude");
            restrictions[i].setSelectedIndex(i);
        }
        
        getTextRestrictionBetween1_1().setVisible(false);
        getTextRestrictionBetween1_1().setDocument(new NumbersOnlyDocument());
        getTextRestrictionBetween1_2().setVisible(false);
        getTextRestrictionBetween1_2().setDocument(new NumbersOnlyDocument());
        getLabelBetween1().setVisible(false);
        
        getTextRestrictionBetween2_1().setVisible(false);
        getTextRestrictionBetween2_1().setDocument(new NumbersOnlyDocument());
        getTextRestrictionBetween2_2().setVisible(false);
        getTextRestrictionBetween2_2().setDocument(new NumbersOnlyDocument());
        getLabelBetween2().setVisible(false);
        
        getTextRestrictionBetween3_1().setVisible(false);
        getTextRestrictionBetween3_1().setDocument(new NumbersOnlyDocument());
        getTextRestrictionBetween3_2().setVisible(false);
        getTextRestrictionBetween3_2().setDocument(new NumbersOnlyDocument());
        getLabelBetween3().setVisible(false);
        
        /**
         * Manage restriction boxes
         */
        updateRestrictions();
        
        /**
         * Attempt to connect to the database.
         */
        dbAccess = new DBAccess();
        if (!dbAccess.isConnected()) {
            JOptionPane.showMessageDialog(this,"Could not connect to DAMCAT database.  Check resources file.", "Error", JOptionPane.ERROR_MESSAGE);
            getButtonSearch().setEnabled(false);
            System.exit(0);
        }
        
        getTableDam().addMouseListener(this);
        
        // user code end
    }
    /**
     * Makes sure the operator list corresponds to the type of field.
     * Creation date: (7/15/2003 7:02:45 AM)
     */
    private void loadOperators() {
        /**
         * The code below could be shortened if all the components were stored in an array
         */
        
        // If the restriciton field is a number, load in the number operators, if it is a string load the string operators
        // (restriction #1)
        if (getComboRestriction1().getSelectedItem().equals("Longitude") || getComboRestriction1().getSelectedItem().equals("Latitude") || getComboRestriction1().getSelectedItem().equals("Max Storage")) {
            if (getComboOperator1().getItemCount() != 4) {
                if (getComboOperator1().getItemCount() != 0)
                    getComboOperator1().removeAllItems();
                getTextRestriction1().setDocument(new NumbersOnlyDocument());
                getComboOperator1().addItem("equals");
                getComboOperator1().addItem("is greater than");
                getComboOperator1().addItem("is less than");
                getComboOperator1().addItem("is between");
            }
        } else {
            if (getComboOperator1().getItemCount() != 2) {
                if (getComboOperator1().getItemCount() != 0)
                    getComboOperator1().removeAllItems();
                getTextRestriction1().setDocument(new PlainDocument());
                getComboOperator1().addItem("contains");
                getComboOperator1().addItem("equals");
            }
        }
        
        // If the restriciton field is a number, load in the number operators, if it is a string load the string operators
        // (restriction #2)
        if (getComboRestriction2().getSelectedItem().equals("Longitude") || getComboRestriction2().getSelectedItem().equals("Latitude") || getComboRestriction2().getSelectedItem().equals("Max Storage")) {
            if (getComboOperator2().getItemCount() != 4) {
                if (getComboOperator2().getItemCount() != 0)
                    getComboOperator2().removeAllItems();
                getTextRestriction2().setDocument(new NumbersOnlyDocument());
                getComboOperator2().addItem("equals");
                getComboOperator2().addItem("is greater than");
                getComboOperator2().addItem("is less than");
                getComboOperator2().addItem("is between");
            }
        } else {
            if (getComboOperator2().getItemCount() != 2) {
                if (getComboOperator2().getItemCount() != 0)
                    getComboOperator2().removeAllItems();
                getTextRestriction2().setDocument(new PlainDocument());
                getComboOperator2().addItem("contains");
                getComboOperator2().addItem("equals");
            }
        }
        
        // If the restriciton field is a number, load in the number operators, if it is a string load the string operators
        // (restriction #3)
        if (getComboRestriction3().getSelectedItem().equals("Longitude") || getComboRestriction3().getSelectedItem().equals("Latitude") || getComboRestriction3().getSelectedItem().equals("Max Storage")) {
            if (getComboOperator3().getItemCount() != 4) {
                if (getComboOperator3().getItemCount() != 0)
                    getComboOperator3().removeAllItems();
                getTextRestriction3().setDocument(new NumbersOnlyDocument());
                getComboOperator3().addItem("equals");
                getComboOperator3().addItem("is greater than");
                getComboOperator3().addItem("is less than");
                getComboOperator3().addItem("is between");
            }
        } else {
            if (getComboOperator3().getItemCount() != 2) {
                if (getComboOperator3().getItemCount() != 0)
                    getComboOperator3().removeAllItems();
                getTextRestriction3().setDocument(new PlainDocument());
                getComboOperator3().addItem("contains");
                getComboOperator3().addItem("equals");
            }
        }
    }
    /**
     * Invoke DAMCAT_Search as a standalone application.
     * Creation date: (7/14/2003 11:04:59 AM)
     * @param args java.lang.String[]
     */
    public static void main(String[] args) {
        System.out.println("Initializing DAMCAT_Search...");
        Search mainFrame = new Search(true);
        mainFrame.setVisible(true);
    }
    /**
     * Adds actionListeners to the combo boxes.
     * Creation date: (7/15/2003 7:40:03 AM)
     * @param bRemove boolean
     */
    private void manageListeners(boolean bRemove) {
        
        if (bRemove) {
            getComboRestriction1().removeActionListener(this);
            getComboRestriction2().removeActionListener(this);
            getComboRestriction3().removeActionListener(this);
            getComboOperator1().removeActionListener(this);
            getComboOperator2().removeActionListener(this);
            getComboOperator3().removeActionListener(this);
        } else {
            getComboRestriction1().addActionListener(this);
            getComboRestriction2().addActionListener(this);
            getComboRestriction3().addActionListener(this);
            getComboOperator1().addActionListener(this);
            getComboOperator2().addActionListener(this);
            getComboOperator3().addActionListener(this);
        }
    }
    /**
     * Allow the user to double click on a dam in the search results table in order to view
     * its information.
     * Creation date: (7/17/2003 1:49:25 PM)
     * @param e java.awt.event.MouseEvent
     */
    public void mouseClicked(MouseEvent e) {
        // if this is a double click, see if the mouse is over a row
        if (e.getClickCount() >= 2 && getTableDam().rowAtPoint(e.getPoint()) != -1) {
            handleViewEdit();
        }
    }
    /**
     * Insert the method's description here.
     * Creation date: (7/17/2003 1:49:37 PM)
     * @param e java.awt.event.MouseEvent
     */
    public void mouseEntered(MouseEvent e) {}
    /**
     * Insert the method's description here.
     * Creation date: (7/17/2003 1:49:44 PM)
     * @param e java.awt.event.MouseEvent
     */
    public void mouseExited(MouseEvent e) {}
    /**
     * Insert the method's description here.
     * Creation date: (7/17/2003 1:49:52 PM)
     * @param e java.awt.event.MouseEvent
     */
    public void mousePressed(MouseEvent e) {}
    /**
     * Insert the method's description here.
     * Creation date: (7/17/2003 1:50:00 PM)
     * @param e java.awt.event.MouseEvent
     */
    public void mouseReleased(MouseEvent e) {}
    private void trace()
    {
        try
        {
            throw new Exception("trace");
        }
        catch(Exception e)
        {
            e.printStackTrace();
        }
    }
    /**
     * Update the restriction entries to reflect user choices.
     * Creation date: (7/14/2003 11:30:44 AM)
     */
    private void updateRestrictions() {
        
        // turn off combo box action listeners for now
        manageListeners(true);
        
        // position "More" and "Fewer" buttons
        if (numRestrictions == 1) {
            getButtonMore().setLocation(317,60);
            getButtonFewer().setLocation(407,60);
        } else if (numRestrictions == 2) {
            getButtonMore().setLocation(317,90);
            getButtonFewer().setLocation(407,90);
        } else {
            getButtonMore().setLocation(317,120);
            getButtonFewer().setLocation(407,120);
        }
        
        // enable / disable "More" and "Fewer" buttons
        if (numRestrictions == 3)
            getButtonMore().setEnabled(false);
        else
            getButtonMore().setEnabled(true);
        
        if (numRestrictions == 1)
            getButtonFewer().setEnabled(false);
        else
            getButtonFewer().setEnabled(true);
        
        // enable / disable editors for the restrictions	
        if (numRestrictions == 1) {
            
            // disable #2
            getComboRestriction2().setVisible(false);
            getComboOperator2().setVisible(false);
            getTextRestrictionBetween2_1().setVisible(false);
            getTextRestrictionBetween2_2().setVisible(false);
            getLabelBetween2().setVisible(false);
            getTextRestriction2().setVisible(false);
            
            // disable #3
            getComboRestriction3().setVisible(false);
            getComboOperator3().setVisible(false);
            getTextRestrictionBetween3_1().setVisible(false);
            getTextRestrictionBetween3_2().setVisible(false);
            getLabelBetween3().setVisible(false);
            getTextRestriction3().setVisible(false);
            
        } else if (numRestrictions == 2) {
            
            // enable #2
            getComboRestriction2().setVisible(true);
            getComboOperator2().setVisible(true);
            getTextRestriction2().setVisible(true);
            
            // disable #3
            getComboRestriction3().setVisible(false);
            getComboOperator3().setVisible(false);
            getTextRestrictionBetween3_1().setVisible(false);
            getTextRestrictionBetween3_2().setVisible(false);
            getLabelBetween3().setVisible(false);
            getTextRestriction3().setVisible(false);
            
        } else if (numRestrictions == 3) {
            
            // enable #2	
            getComboRestriction2().setVisible(true);
            getComboOperator3().setVisible(true);
            getTextRestriction2().setVisible(true);
            
            // enable #3
            getComboRestriction3().setVisible(true);
            getComboOperator3().setVisible(true);
            getTextRestriction3().setVisible(true);
            
        }
        
        // handle the different search methods for strings and numbers
        loadOperators();
        
        // enable / disable the "between" text fields (restriction #1)
        if (getComboOperator1().getSelectedItem().equals("is between")) {
            getTextRestriction1().setVisible(false);
            getLabelBetween1().setVisible(true);
            getTextRestrictionBetween1_1().setVisible(true);
            getTextRestrictionBetween1_2().setVisible(true);
        } else {
            getTextRestriction1().setVisible(true);
            getLabelBetween1().setVisible(false);
            getTextRestrictionBetween1_1().setVisible(false);
            getTextRestrictionBetween1_2().setVisible(false);
        }
        
        // enable / disable the "between" text fields (restriction #2)
        if (getComboOperator2().getSelectedItem().equals("is between") && numRestrictions >= 2) {
            getTextRestriction2().setVisible(false);
            getLabelBetween2().setVisible(true);
            getTextRestrictionBetween2_1().setVisible(true);
            getTextRestrictionBetween2_2().setVisible(true);
        } else if (numRestrictions >= 2) {
            getTextRestriction2().setVisible(true);
            getLabelBetween2().setVisible(false);
            getTextRestrictionBetween2_1().setVisible(false);
            getTextRestrictionBetween2_2().setVisible(false);
        }
        
        // enable / disable the "between" text fields (restriction #3)
        if (getComboOperator3().getSelectedItem().equals("is between") && numRestrictions >= 3) {
            getTextRestriction3().setVisible(false);
            getLabelBetween3().setVisible(true);
            getTextRestrictionBetween3_1().setVisible(true);
            getTextRestrictionBetween3_2().setVisible(true);
        } else if (numRestrictions >= 3) {
            getTextRestriction3().setVisible(true);
            getLabelBetween3().setVisible(false);
            getTextRestrictionBetween3_1().setVisible(false);
            getTextRestrictionBetween3_2().setVisible(false);
        }
        
        // turn combo box action listeners back on
        manageListeners(false);
    }
    /**
     * Listenes for dam info click.
     * Creation date: (7/15/2003 11:22:19 AM)
     */
    public void valueChanged(ListSelectionEvent e) {
        if (!e.getValueIsAdjusting()) {
            if (getTableDam().getSelectedRow() == -1) {
                getButtonViewEdit().setEnabled(false);
                getButtonExport().setEnabled(false);
                getButtonCloneDam().setEnabled(false);
                getButtonDeleteADam().setEnabled(false);
                getButtonPrerun().setEnabled(false);
            }
            else {
                getButtonViewEdit().setEnabled(true);
                getButtonExport().setEnabled(true);
                getButtonCloneDam().setEnabled(true);
                getButtonDeleteADam().setEnabled(true);
                getButtonPrerun().setEnabled(true);
            }
        }
    }
}
