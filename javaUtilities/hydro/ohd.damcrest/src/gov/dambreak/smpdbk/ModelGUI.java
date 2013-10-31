package gov.dambreak.smpdbk;

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.border.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;
import gov.dambreak.util.*;
import gov.dambreak.menu.*;
import gov.damcat.data.*;

/**
 * GUI For Model Manipulation Creation date: (7/29/2003 8:26:32 AM)
 * 
 * @author:
 */
public class ModelGUI extends JFrame
{
    // GUI Data Fields
    private AnalysisData      currentData                  = new AnalysisData();
    // to declare outputOnlyData for handleMenuDisplayOutput()
    private AnalysisData      outputOnlyData               = new AnalysisData();
    private String            strOpenedFilename;
    private String            strLastOpenPath;

    //	private boolean _DEBUG = false;
    private boolean           _DEBUG                       = true;
    private int               nScenarioEditing             = -1;
    private int               nDownstreamEditing           = -1;
    private int               nXSEditing                   = -1;
    private boolean           bHandlingEvent               = false;             // prevents
                                                                                // multiple
                                                                                // Events
                                                                                // from
                                                                                // firing

    private boolean           bHasDatabase                 = false;             // passed
                                                                                // in
                                                                                // to
                                                                                // constructor;
                                                                                // indicates
                                                                                // whether
                                                                                // DBMS
                                                                                // is
                                                                                // in
                                                                                // use

    private String            strWorkingDamNIDID;
    private DBAccess          dbAccess;
    private JComboBox         ivjcomboScenario             = null;
    private JComboBox         ivjcomboTypeOfDam            = null;
    private JComboBox         ivjcomboXSType               = null;
    private JPanel            ivjJFrameContentPane         = null;
    private JSeparator        ivjJSeparator1               = null;
    private JSeparator        ivjJSeparator2               = null;
    private JLabel            ivjlabelBME                  = null;
    private JLabel            ivjlabelBW                   = null;
    private JLabel            ivjlabelCMS                  = null;
    private JLabel            ivjlabelDamName              = null;
    private JLabel            ivjlabelDISTTN               = null;
    private JLabel            ivjlabelHDE                  = null;
    private JLabel            ivjlabelPPOI                 = null;
    private JLabel            ivjlabelQO                   = null;
    private JLabel            ivjlabelRequiredFieldsNotice = null;
    private JLabel            ivjlabelRiverName            = null;
    private JLabel            ivjlabelSA                   = null;
    private JLabel            ivjlabelScenario             = null;
    private JLabel            ivjlabelScenarioList         = null;
    private JLabel            ivjlabelSource               = null;
    private JLabel            ivjlabelTFM                  = null;
    private JLabel            ivjlabelTypeOfDam            = null;
    private JLabel            ivjlabelVolume               = null;
    private JLabel            ivjlabelXSDistance           = null;
    private JLabel            ivjlabelXSFloodDepth         = null;
    private JLabel            ivjlabelXSLatitutde          = null;
    private JLabel            ivjlabelXSList               = null;
    private JLabel            ivjlabelXSList1              = null;
    private JLabel            ivjlabelXSLongitude          = null;
    private JLabel            ivjlabelXSName               = null;
    private JMenuItem         ivjmenuAbout                 = null;
    private JMenu             ivjmenuFile                  = null;
    private JMenu             ivjmenuHelp                  = null;
    private JMenuItem         ivjmenuImportDAT             = null;
    private JMenuItem         ivjmenuImportFLDXS           = null;
    private JMenuItem         ivjmenuNew                   = null;
    private JMenuItem         ivjmenuOpen                  = null;
    private JMenuItem         ivjmenuQuit                  = null;
    private JMenuItem         ivjmenuSave                  = null;
    private JMenuItem         ivjmenuSaveAs                = null;
    private JMenu             ivjmenuSMPDBK                = null;
    private JMenuBar          ivjModelGUIJMenuBar          = null;
    private JPanel            ivjpanelDamInfo              = null;
    private JScrollPane       ivjscrollScenarioList        = null;
    private JScrollPane       ivjscrollXSData              = null;
    private JScrollPane       ivjscrollXSList              = null;
    private JScrollPane       ivjscrollXSList1             = null;
    private JTable            ivjtableXSData               = null;
    private JTextField        ivjtextBME                   = null;
    private JTextField        ivjtextBW                    = null;
    private JTextField        ivjtextCMS                   = null;
    private JTextField        ivjtextDamName               = null;
    private JTextField        ivjtextDISTTN                = null;
    private JTextField        ivjtextFloodFlow             = null;
    private JTextField        ivjtextHDE                   = null;
    private JTextField        ivjtextPPOI                  = null;
    private JTextField        ivjtextQO                    = null;
    private JTextField        ivjtextRiverName             = null;
    private JTextField        ivjtextSA                    = null;
    private JTextField        ivjtextSource                = null;
    private JTextField        ivjtextTFM                   = null;
    private JTextField        ivjtextVOL                   = null;
    private DefaultListModel  ivjlistModelDown             = null;
    private DefaultListModel  ivjlistModelScenario         = null;
    private DefaultListModel  ivjlistModelXS               = null;
    private JList             ivjListDown                  = null;
    private JList             ivjListScenario              = null;
    private JList             ivjListXS                    = null;
    private JTextField        ivjtextDistance              = null;
    private JTextField        ivjtextFloodDepth            = null;
    private JTextField        ivjtextLatitude              = null;
    private JTextField        ivjtextLongitude             = null;
    private JTextField        ivjtextName                  = null;
    private TableColumn       ivjcolumnBS                  = null;
    private TableColumn       ivjcolumnBSS                 = null;
    private TableColumn       ivjcolumnElevation           = null;
    private TableColumn       ivjcolumnMannN               = null;
    private DefaultTableModel ivjtableModelXSGeom          = null;
    private JLabel            ivjlabelXSList11             = null;
    private JMenuItem         ivjmenuClose                 = null;
    private JMenu             ivjmenuDatabase              = null;
    private JMenuItem         ivjmenuUpdateDatabase        = null;
    private JLabel            ivjlabelSectionType          = null;
    private JLabel            ivjlabelXSFloodFlow          = null;
    private JPanel            ivjpanelDownstreamInfo       = null;
    private JPanel            ivjpanelNameInfo             = null;
    private JPanel            ivjpanelXS                   = null;
    private JMenu             ivjmenuEdit                  = null;
    private JMenuItem         ivjmenuEditText              = null;
    private JMenuItem         ivjmenuDisplayOutput         = null;
    private JMenuItem         ivjmenuRun_SMPDBK            = null;
    private JComboBox         ivjcomboDBUG                 = null;
    private JLabel            ivjlabelDBUG                 = null;
    private boolean           bHasDataToSave               = false;
    private JComboBox         ivjcomboBestXS               = null;
    private int               _selectedScenario            = -1;
    private int               _selectedDownstreamPoint     = -1;
    private int               _selectedXSType              = -1;
    private Search            searchTool;
    private boolean           bChangedData                 = false;

    //private JTextField ivjBestTypeText = null;
    /**
     * Constructor
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public ModelGUI()
    {
        super();
        initialize();
    }

    /**
     * Insert the method's description here. Creation date: (7/30/2003 1:25:47
     * PM)
     * 
     * @param dbData
     *            util.AnalysisData
     */
    public ModelGUI(boolean _bHasDatabase, String _strWorkingDamNIDID,
            AnalysisData _currentData, DBAccess _dbAccess)
    {

        super();
        System.out.println("Initializing SMPDBK model input editor...");

        currentData = _currentData;
        if (currentData == null)
        {
            currentData = new AnalysisData();
            bHasDataToSave = false;
        } else
        {
            bHasDataToSave = true;
        }
        strWorkingDamNIDID = _strWorkingDamNIDID;
        strOpenedFilename = null;
        strLastOpenPath = "";
        bHasDatabase = _bHasDatabase;
        dbAccess = _dbAccess;

        initialize();

        if (bHasDatabase)
            setTitle("Model Input Editor - Database Mode - Editing DAMCAT Entry: "
                    + _strWorkingDamNIDID);
        else
            setTitle("Model Input Editor - File Mode");

//        show();
        this.setVisible( true );
        java.awt.Insets insets = getInsets();
        setSize(getWidth() + insets.left + insets.right, getHeight()
                + insets.top + insets.bottom);
        setVisible(true);
    }

    /**
     * Provide dialogs to ask user if he wants to save Creation date: (8/1/2003
     * 12:09:47 PM)
     * 
     * @return boolean
     */
    private boolean checkForSaveChanges()
    {

        int confirmOption;

        if (bChangedData == true)
        {
            if (bHasDatabase)
            {

                // ask the user if they would like to save before exiting

                confirmOption = JOptionPane.showConfirmDialog(this,
                        "Do you want to save changes to the database ?",
                        "Confirm", JOptionPane.YES_NO_OPTION);

                if (confirmOption == JOptionPane.YES_OPTION)
                {
                    // If the database update fails, return false
                    if (handleMenuUpdateDatabase(false) == false)
                        return false;
                }

            } else
            {

                // ask the user if they would like to save before exiting

                confirmOption = JOptionPane.showConfirmDialog(this,
                        "Do you want to save any changes to a file ?",
                        "Confirm", JOptionPane.YES_NO_OPTION);

                if (confirmOption == JOptionPane.YES_OPTION)
                {
                    // If the save fails, return false to let the calling method
                    // know that
                    // the user's wish was not granted
                    if (handleMenuSave() == false)
                        return false;
                }

            }
        }

        bChangedData = false;

        return true;
    }

    /**
     * Initialize GUI for Downstream Point
     */
    private void clearDownstreamFields()
    {

        gettextName().setText("");
        gettextDistance().setText("");
        gettextFloodDepth().setText("");
        gettextFloodFlow().setText("");
        gettextLatitude().setText("");
        gettextLongitude().setText("");
        // if (_DEBUG) System.out.println("Item Count is: " +
        // getcomboBestXS().getItemCount());
        if (getcomboBestXS().getItemCount() > 0)
            getcomboBestXS().setSelectedIndex(0);

        getlistModelXS().clear();
        getListXS().clearSelection();

        clearXSFields();

        getcomboXSType().setSelectedIndex(0);
        gettableXSData().clearSelection();

        /*
         * while (gettableModelXSGeom().getRowCount() != 0)
         * gettableModelXSGeom().removeRow(0);
         */

        nDownstreamEditing = -1;

    }

    /**
     * Initialize ModelGUI for Input Scenario fields Creation date: (7/29/2003
     * 10:14:27 AM)
     */
    private void clearScenarioFields()
    {

        gettextSource().setText("");
        getcomboScenario().setSelectedIndex(0);
        getcomboTypeOfDam().setSelectedIndex(0);
        getcomboDBUG().setSelectedIndex(0);
        gettextHDE().setText("");
        gettextBME().setText("");
        gettextVOL().setText("");
        gettextSA().setText("");
        gettextBW().setText("");
        gettextTFM().setText("");
        gettextQO().setText("");
        gettextDISTTN().setText("");
        gettextCMS().setText("");

        nScenarioEditing = -1;

    }

    /**
     * Insert the method's description here. Creation date: (7/29/2003 3:25:56
     * PM)
     */
    private void clearXSFields()
    {

        getcomboXSType().setSelectedIndex(0);
        stopTableEditing();
        gettableXSData().clearSelection();

        int rowLimiter = gettableModelXSGeom().getRowCount();
        for (int i = 0; i < rowLimiter; i++)
        {
            gettableModelXSGeom().setValueAt("", i, 0);
            gettableModelXSGeom().setValueAt("", i, 1);
            gettableModelXSGeom().setValueAt("", i, 2);
            gettableModelXSGeom().setValueAt("", i, 3);
        }

        nXSEditing = -1;
    }

    /**
     * connEtoC1:
     * (ListScenario.listSelection.valueChanged(javax.swing.event.ListSelectionEvent)
     * --> ModelGUI.handleScenarioListSelection()V)
     * 
     * @param arg1
     *            javax.swing.event.ListSelectionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC1( ListSelectionEvent arg1 )
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleScenarioListSelection(arg1);
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC10: (textName.focus.focusLost(java.awt.event.FocusEvent) -->
     * ModelGUI.handleDownstreamKeyLostFocus()V)
     * 
     * @param arg1
     *            java.awt.event.FocusEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC10(java.awt.event.FocusEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleDownstreamKeyLostFocus();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC11: (textDistance.focus.focusLost(java.awt.event.FocusEvent) -->
     * ModelGUI.handleDownstreamKeyLostFocus()V)
     * 
     * @param arg1
     *            java.awt.event.FocusEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC11(java.awt.event.FocusEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleDownstreamKeyLostFocus();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC12:
     * (ListXS.listSelection.valueChanged(javax.swing.event.ListSelectionEvent)
     * -->
     * ModelGUI.handleXSListSelection(Ljavax.swing.event.ListSelectionEvent;)V)
     * 
     * @param arg1
     *            javax.swing.event.ListSelectionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC12( ListSelectionEvent arg1 )
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleXSListSelection(arg1);
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC13:
     * (comboXSType.action.actionPerformed(java.awt.event.ActionEvent) -->
     * ModelGUI.handleXSKeyLostFocus()V)
     * 
     * @param arg1
     *            java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC13(java.awt.event.ActionEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleXSKeyLostFocus();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC14: (ListXS.mouse.mouseClicked(java.awt.event.MouseEvent) -->
     * ModelGUI.handleXSListClick(Ljava.awt.event.MouseEvent;)V)
     * 
     * @param arg1
     *            java.awt.event.MouseEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC14(java.awt.event.MouseEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleXSListClick(arg1);
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC15: (ModelGUI.window.windowClosing(java.awt.event.WindowEvent)
     * --> ModelGUI.handleWindowClosing()V)
     * 
     * @param arg1
     *            java.awt.event.WindowEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC15(java.awt.event.WindowEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleWindowClosing();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC16: (menuClose.action.actionPerformed(java.awt.event.ActionEvent)
     * --> ModelGUI.handleMenuClose()V)
     * 
     * @param arg1
     *            java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC16(java.awt.event.ActionEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleMenuClose();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC17: (comboBestXS.action. -->
     * ModelGUI.handleDownstreamKeyLostFocus()V)
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC17()
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleDownstreamKeyLostFocus();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC18: (menuAbout.action.actionPerformed(java.awt.event.ActionEvent)
     * --> ModelGUI.handleMenuAbout()V)
     * 
     * @param arg1
     *            java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC18(java.awt.event.ActionEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleMenuAbout();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC19:
     * (menuImportDAT.action.actionPerformed(java.awt.event.ActionEvent) -->
     * ModelGUI.handleMenuImportFromDAT()V)
     * 
     * @param arg1
     *            java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC19(java.awt.event.ActionEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleMenuImportFromDAT();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC2:
     * (ListDown.listSelection.valueChanged(javax.swing.event.ListSelectionEvent)
     * --> ModelGUI.handleDownstreamListSelection()V)
     * 
     * @param arg1
     *            javax.swing.event.ListSelectionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC2( ListSelectionEvent arg1 )
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleDownstreamListSelection(arg1);
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC20:
     * (menuImportFLDXS.action.actionPerformed(java.awt.event.ActionEvent) -->
     * ModelGUI.handleMenuImportFromFLDXS()V)
     * 
     * @param arg1
     *            java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC20(java.awt.event.ActionEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleMenuImportFromFLDXS();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC21:
     * (menuUpdateDatabase.action.actionPerformed(java.awt.event.ActionEvent)
     * --> ModelGUI.handleMenuUpdateDatabase(Z)Z)
     * 
     * @return boolean
     * @param arg1
     *            java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private boolean connEtoC21(java.awt.event.ActionEvent arg1)
    {
        boolean connEtoC21Result = false;
        try
        {
            // user code begin {1}
            // user code end
            connEtoC21Result = this.handleMenuUpdateDatabase(true);
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
        return connEtoC21Result;
    }

    /**
     * connEtoC22: (menuSave.action.actionPerformed(java.awt.event.ActionEvent)
     * --> ModelGUI.handleMenuSave()V)
     * 
     * @param arg1
     *            java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC22(java.awt.event.ActionEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleMenuSave();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC23: (menuOpen.action.actionPerformed(java.awt.event.ActionEvent)
     * --> ModelGUI.handleMenuOpen()Z)
     * 
     * @return boolean
     * @param arg1
     *            java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private boolean connEtoC23(java.awt.event.ActionEvent arg1)
    {
        boolean connEtoC23Result = false;
        try
        {
            // user code begin {1}
            // user code end
            connEtoC23Result = this.handleMenuOpen();
            // user code begin {2}
            // System.out.println("The connEtoC23Result is:" +
            // connEtoC23Result);
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            System.out.println("Throwable of connEtoC23");
            // user code end
            handleException(ivjExc);
        }
        return connEtoC23Result;
    }

    /**
     * connEtoC24:
     * (menuSaveAs.action.actionPerformed(java.awt.event.ActionEvent) -->
     * ModelGUI.handleMenuSaveAs()Z)
     * 
     * @return boolean
     * @param arg1
     *            java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private boolean connEtoC24(java.awt.event.ActionEvent arg1)
    {
        boolean connEtoC24Result = false;
        try
        {
            // user code begin {1}
            // user code end
            connEtoC24Result = this.handleMenuSaveAs();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
        return connEtoC24Result;
    }

    /**
     * connEtoC25:
     * (menuEditText.action.actionPerformed(java.awt.event.ActionEvent) -->
     * ModelGUI.handleMenuEditText()V)
     * 
     * @param arg1
     *            java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC25(java.awt.event.ActionEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleMenuEditText();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC26:
     * (menuDisplayOutput.action.actionPerformed(java.awt.event.ActionEvent) -->
     * ModelGUI.handleDisplayOutput()V)
     * 
     * @param arg1
     *            java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC26(java.awt.event.ActionEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleMenuDisplayOutput();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC27:
     * (menuRun_SMPDBK.action.actionPerformed(java.awt.event.ActionEvent) -->
     * ModelGUI.handleMenuRun_SMPDBK()V)
     * 
     * @param arg1
     *            java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC27(java.awt.event.ActionEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleMenuRun_SMPDBK();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC3: (ListXS.key.keyPressed(java.awt.event.KeyEvent) -->
     * ModelGUI.handleXSListKey(Ljava.awt.event.KeyEvent;)V)
     * 
     * @param arg1
     *            java.awt.event.KeyEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC3(java.awt.event.KeyEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleXSListKey(arg1);
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC4: (menuQuit.action.actionPerformed(java.awt.event.ActionEvent)
     * --> ModelGUI.handleMenuQuit()V)
     * 
     * @param arg1
     *            java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC4(java.awt.event.ActionEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleMenuQuit();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC5: (ListScenario.key.keyPressed(java.awt.event.KeyEvent) -->
     * ModelGUI.handleScenarioListKey(Ljava.awt.event.KeyEvent;)V)
     * 
     * @param arg1
     *            java.awt.event.KeyEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC5(java.awt.event.KeyEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleScenarioListKey(arg1);
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC6:
     * (comboScenario.action.actionPerformed(java.awt.event.ActionEvent) -->
     * ModelGUI.handleScenarioKeyFocusLost()V)
     * 
     * @param arg1
     *            java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC6(java.awt.event.ActionEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleScenarioKeyFocusLost();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC7: (textSource.focus.focusLost(java.awt.event.FocusEvent) -->
     * ModelGUI.handleScenarioKeyFocusLost()V)
     * 
     * @param arg1
     *            java.awt.event.FocusEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC7(java.awt.event.FocusEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleScenarioKeyFocusLost();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC8: (menuNew.action.actionPerformed(java.awt.event.ActionEvent)
     * --> ModelGUI.handleMenuNew()V)
     * 
     * @param arg1
     *            java.awt.event.ActionEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC8(java.awt.event.ActionEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleMenuNew();
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connEtoC9: (ListDown.key.keyPressed(java.awt.event.KeyEvent) -->
     * ModelGUI.handleDownListKey(Ljava.awt.event.KeyEvent;)V)
     * 
     * @param arg1
     *            java.awt.event.KeyEvent
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connEtoC9(java.awt.event.KeyEvent arg1)
    {
        try
        {
            // user code begin {1}
            // user code end
            this.handleDownListKey(arg1);
            // user code begin {2}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connPtoP1SetTarget: (listModelXS.this <-->ListXSList.model)
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connPtoP1SetTarget()
    {
        /* Set the target from the source */
        try
        {
            getListXS().setModel(getlistModelXS());
            // user code begin {1}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connPtoP2SetTarget: (listModelDown.this <-->ListDownList.model)
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connPtoP2SetTarget()
    {
        /* Set the target from the source */
        try
        {
            getListDown().setModel(getlistModelDown());
            // user code begin {1}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connPtoP3SetTarget: (listModelScenario.this <-->ListScenarioList.model)
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connPtoP3SetTarget()
    {
        /* Set the target from the source */
        try
        {
            getListScenario().setModel(getlistModelScenario());
            // user code begin {1}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * connPtoP4SetTarget: (tableModelXSGeom.this <-->tableXSData.model)
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void connPtoP4SetTarget()
    {
        /* Set the target from the source */
        try
        {
            gettableXSData().setModel(gettableModelXSGeom());
            // user code begin {1}
            // user code end
        } catch (java.lang.Throwable ivjExc)
        {
            // user code begin {3}
            // user code end
            handleException(ivjExc);
        }
    }

    /**
     * Creates a new downstream point with default data and makes it the current
     * selection Creation date: (7/29/2003 9:06:27 AM)
     */
    private void createNewDownstreamPoint(int selectedIndex)
    {

        DownstreamPoint newDownstream = new DownstreamPoint();

        newDownstream.changeFlag = 2; // *** indicate that DownstreamPoint needs
                                      // inserted !

        currentData.downstream.add(selectedIndex, newDownstream);

        updateDownstreamList();
    }

    /**
     * Creates a new scenario with default data and makes it the current
     * selection Creation date: (7/29/2003 9:06:27 AM)
     */
    private void createNewScenario()
    {

        ModelScenario newScenario = new ModelScenario();

        currentData.scenarios.add(newScenario); // *** this is at bottom

        updateScenarioList();

        bHasDataToSave = true;
    }

    /**
     * Create new SectionGeometry for particular DownstreamPoint Creation date:
     * (7/30/2003 8:02:24 AM)
     */
    private void createNewXS(int whichDownstreamPoint)
    {

        if (whichDownstreamPoint == -1)
            return;

        SectionGeometry newSection = new SectionGeometry();
        DownstreamPoint downEditing = (DownstreamPoint) currentData.downstream
                .get(whichDownstreamPoint);

        // *** add newSection to back of ArrayList
        downEditing.xsections.add(newSection);

        // make this the "best" cross section if it is the only one
        if (downEditing.xsections.size() == 1)
            downEditing.bestXS = 0;

        updateXSList();
    }

    /**
     * Insert the method's description here. Creation date: (7/29/2003 12:10:26
     * PM)
     * 
     * @param bEnable
     *            boolean
     */
    private void enableDownstreamEditors(boolean bEnable)
    {
        gettextName().setEnabled(bEnable);
        gettextDistance().setEnabled(bEnable);
        gettextFloodDepth().setEnabled(bEnable);
        gettextFloodFlow().setEnabled(bEnable);
        gettextLatitude().setEnabled(bEnable);
        gettextLongitude().setEnabled(bEnable);
        getcomboBestXS().setEnabled(bEnable);
        getListXS().setEnabled(bEnable);
    }

    /**
     * Insert the method's description here. Creation date: (7/29/2003 10:17:09
     * AM)
     * 
     * @param bEnable
     *            boolean
     */
    private void enableScenarioEditors(boolean bEnable)
    {
        gettextSource().setEnabled(bEnable);
        getcomboScenario().setEnabled(bEnable);
        getcomboTypeOfDam().setEnabled(bEnable);
        getcomboDBUG().setEnabled(bEnable);
        gettextHDE().setEnabled(bEnable);
        gettextBME().setEnabled(bEnable);
        gettextVOL().setEnabled(bEnable);
        gettextSA().setEnabled(bEnable);
        gettextBW().setEnabled(bEnable);
        gettextTFM().setEnabled(bEnable);
        gettextQO().setEnabled(bEnable);
        gettextDISTTN().setEnabled(bEnable);
        gettextCMS().setEnabled(bEnable);
    }

    /**
     * Insert the method's description here. Creation date: (7/30/2003 8:01:34
     * AM)
     * 
     * @param bEnable
     *            boolean
     */
    private void enableXSEditors(boolean bEnable)
    {
        gettableXSData().setEnabled(bEnable);
        getcomboXSType().setEnabled(bEnable);
    }

    /**
     * This method attempts to fill the output fields of each scenario with
     * updated model output.
     * 
     * If the model fails to run, any existing output is kept as it was.
     * 
     * NOTE: In order to keep input and output synchronized, This should be
     * changed to clear the output if the model fails to run.
     * 
     * Creation date: (7/31/2003 11:00:39 AM)
     */
    private boolean generateOutput()
    {

        boolean generationResult;
        if (_DEBUG)
            System.out.print("Generating output...");
        generationResult = currentData.generateOutput();
        if (_DEBUG)
            System.out.println("Finished.");
        return generationResult;

    }

    /**
     * Return the columnBS property value.
     * 
     * @return javax.swing.table.TableColumn
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.table.TableColumn getcolumnBS()
    {
        if (ivjcolumnBS == null)
        {
            try
            {
                ivjcolumnBS = new javax.swing.table.TableColumn();
                ivjcolumnBS.setModelIndex(1);
                ivjcolumnBS.setHeaderValue("Channel Width");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjcolumnBS;
    }

    /**
     * Return the columnBSS property value.
     * 
     * @return javax.swing.table.TableColumn
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.table.TableColumn getcolumnBSS()
    {
        if (ivjcolumnBSS == null)
        {
            try
            {
                ivjcolumnBSS = new javax.swing.table.TableColumn();
                ivjcolumnBSS.setModelIndex(2);
                ivjcolumnBSS.setHeaderValue("Inactive Width");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjcolumnBSS;
    }

    /**
     * Return the columnElevation property value.
     * 
     * @return javax.swing.table.TableColumn
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.table.TableColumn getcolumnElevation()
    {
        if (ivjcolumnElevation == null)
        {
            try
            {
                ivjcolumnElevation = new javax.swing.table.TableColumn();
                ivjcolumnElevation.setHeaderValue("Elevation");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjcolumnElevation;
    }

    /**
     * Return the columnMannN property value.
     * 
     * @return javax.swing.table.TableColumn
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.table.TableColumn getcolumnMannN()
    {
        if (ivjcolumnMannN == null)
        {
            try
            {
                ivjcolumnMannN = new javax.swing.table.TableColumn();
                ivjcolumnMannN.setModelIndex(3);
                ivjcolumnMannN.setHeaderValue("Manning N");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjcolumnMannN;
    }

    /**
     * Return the comboBestXS property value.
     * 
     * @return javax.swing.JComboBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JComboBox getcomboBestXS()
    {
        if (ivjcomboBestXS == null)
        {
            try
            {
                ivjcomboBestXS = new javax.swing.JComboBox();
                ivjcomboBestXS.setName("comboBestXS");
                ivjcomboBestXS.setFont(new java.awt.Font("dialog", 0, 12));
                ivjcomboBestXS.setBackground(new java.awt.Color(255, 255, 249));
                ivjcomboBestXS.setBounds(234, 297, 143, 22);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjcomboBestXS;
    }

    /**
     * Return the comboDBUG property value.
     * 
     * @return javax.swing.JComboBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JComboBox getcomboDBUG()
    {
        if (ivjcomboDBUG == null)
        {
            try
            {
                ivjcomboDBUG = new javax.swing.JComboBox();
                ivjcomboDBUG.setName("comboDBUG");
                ivjcomboDBUG.setFont(new java.awt.Font("dialog", 0, 12));
                ivjcomboDBUG.setBackground(new java.awt.Color(255, 255, 255));
                ivjcomboDBUG.setBounds(269, 437, 125, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjcomboDBUG;
    }

    /**
     * Return the comboScenario property value.
     * 
     * @return javax.swing.JComboBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JComboBox getcomboScenario()
    {
        if (ivjcomboScenario == null)
        {
            try
            {
                ivjcomboScenario = new javax.swing.JComboBox();
                ivjcomboScenario.setName("comboScenario");
                ivjcomboScenario.setFont(new java.awt.Font("dialog", 0, 12));
                ivjcomboScenario
                        .setBackground(new java.awt.Color(255, 255, 255));
                ivjcomboScenario.setBounds(269, 144, 125, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjcomboScenario;
    }

    /**
     * Return the comboTypeOfDam property value.
     * 
     * @return javax.swing.JComboBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JComboBox getcomboTypeOfDam()
    {
        if (ivjcomboTypeOfDam == null)
        {
            try
            {
                ivjcomboTypeOfDam = new javax.swing.JComboBox();
                ivjcomboTypeOfDam.setName("comboTypeOfDam");
                ivjcomboTypeOfDam.setFont(new java.awt.Font("dialog", 0, 12));
                ivjcomboTypeOfDam.setBackground(new java.awt.Color(255, 255,
                        255));
                ivjcomboTypeOfDam.setBounds(269, 168, 125, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjcomboTypeOfDam;
    }

    /**
     * Return the comboXSType property value.
     * 
     * @return javax.swing.JComboBox
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JComboBox getcomboXSType()
    {
        if (ivjcomboXSType == null)
        {
            try
            {
                ivjcomboXSType = new javax.swing.JComboBox();
                ivjcomboXSType.setName("comboXSType");
                ivjcomboXSType.setFont(new java.awt.Font("dialog", 0, 12));
                ivjcomboXSType.setBackground(new java.awt.Color(255, 255, 255));
                ivjcomboXSType.setBounds(138, 94, 209, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjcomboXSType;
    }

    /**
     * Return the JFrameContentPane property value.
     * 
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private JPanel getJFrameContentPane()
    {
        if (ivjJFrameContentPane == null)
        {
            try
            {
                ivjJFrameContentPane = new javax.swing.JPanel();
                ivjJFrameContentPane.setName("JFrameContentPane");
                ivjJFrameContentPane.setLayout(null);
                getJFrameContentPane().add(getpanelNameInfo(),
                        getpanelNameInfo().getName());
                getJFrameContentPane().add(getpanelDamInfo(),
                        getpanelDamInfo().getName());
                getJFrameContentPane().add(getpanelDownstreamInfo(),
                        getpanelDownstreamInfo().getName());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjJFrameContentPane;
    }

    /**
     * Return the JSeparator1 property value.
     * 
     * @return javax.swing.JSeparator
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JSeparator getJSeparator1()
    {
        if (ivjJSeparator1 == null)
        {
            try
            {
                ivjJSeparator1 = new javax.swing.JSeparator();
                ivjJSeparator1.setName("JSeparator1");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjJSeparator1;
    }

    /**
     * Return the JSeparator2 property value.
     * 
     * @return javax.swing.JSeparator
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JSeparator getJSeparator2()
    {
        if (ivjJSeparator2 == null)
        {
            try
            {
                ivjJSeparator2 = new javax.swing.JSeparator();
                ivjJSeparator2.setName("JSeparator2");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjJSeparator2;
    }

    /**
     * Return the labelBestXS property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    /*
     * private javax.swing.JLabel getlabelBestXS() { if (ivjlabelBestXS == null) {
     * try { ivjlabelBestXS = new javax.swing.JLabel();
     * ivjlabelBestXS.setName("labelBestXS"); ivjlabelBestXS.setFont(new
     * java.awt.Font("Arial", 1, 10)); ivjlabelBestXS.setText("");
     * ivjlabelBestXS.setBounds(312, 10, 24, 14);
     * ivjlabelBestXS.setForeground(java.awt.Color.black); // user code begin
     * {1} // user code end } catch (java.lang.Throwable ivjExc) { // user code
     * begin {2} // user code end handleException(ivjExc); } } return
     * ivjlabelBestXS; }
     */
    /**
     * Return the labelBME property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelBME()
    {
        if (ivjlabelBME == null)
        {
            try
            {
                ivjlabelBME = new javax.swing.JLabel();
                ivjlabelBME.setName("labelBME");
                ivjlabelBME.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelBME.setText("* Final Breach Elevation (BME in FT MSL)");
                ivjlabelBME.setBounds(13, 227, 230, 14);
                ivjlabelBME.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelBME;
    }

    /**
     * Return the labelBW property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelBW()
    {
        if (ivjlabelBW == null)
        {
            try
            {
                ivjlabelBW = new javax.swing.JLabel();
                ivjlabelBW.setName("labelBW");
                ivjlabelBW.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelBW.setText("Final Breach Width (BW in FT)");
                ivjlabelBW.setBounds(13, 307, 226, 14);
                ivjlabelBW.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelBW;
    }

    /**
     * Return the labelCMS property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelCMS()
    {
        if (ivjlabelCMS == null)
        {
            try
            {
                ivjlabelCMS = new javax.swing.JLabel();
                ivjlabelCMS.setName("labelCMS");
                ivjlabelCMS.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelCMS.setText("Dead Storage Equivalent Mann. N (CMS)");
                ivjlabelCMS.setBounds(13, 413, 226, 14);
                ivjlabelCMS.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelCMS;
    }

    /**
     * Return the labelDamName property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelDamName()
    {
        if (ivjlabelDamName == null)
        {
            try
            {
                ivjlabelDamName = new javax.swing.JLabel();
                ivjlabelDamName.setName("labelDamName");
                ivjlabelDamName.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelDamName.setText("* Dam Name");
                ivjlabelDamName.setBounds(17, 15, 124, 14);
                ivjlabelDamName.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelDamName;
    }

    /**
     * Return the labelDBUG property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelDBUG()
    {
        if (ivjlabelDBUG == null)
        {
            try
            {
                ivjlabelDBUG = new javax.swing.JLabel();
                ivjlabelDBUG.setName("labelDBUG");
                ivjlabelDBUG.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelDBUG.setText("SMPDBK Debug Option");
                ivjlabelDBUG.setBounds(13, 440, 226, 14);
                ivjlabelDBUG.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelDBUG;
    }

    /**
     * Return the labelDISTTN property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelDISTTN()
    {
        if (ivjlabelDISTTN == null)
        {
            try
            {
                ivjlabelDISTTN = new javax.swing.JLabel();
                ivjlabelDISTTN.setName("labelDISTTN");
                ivjlabelDISTTN.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelDISTTN
                        .setText("Distance to PT of Interest (DISTTN in MILES)");
                ivjlabelDISTTN.setBounds(13, 386, 253, 14);
                ivjlabelDISTTN.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelDISTTN;
    }

    /**
     * Return the labelHDE property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelHDE()
    {
        if (ivjlabelHDE == null)
        {
            try
            {
                ivjlabelHDE = new javax.swing.JLabel();
                ivjlabelHDE.setName("labelHDE");
                ivjlabelHDE.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelHDE.setText("* Dam Breach Elevation (HDE in FT MSL)");
                ivjlabelHDE.setBounds(13, 199, 231, 14);
                ivjlabelHDE.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelHDE;
    }

    /**
     * Return the labelPPOI property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelPPOI()
    {
        if (ivjlabelPPOI == null)
        {
            try
            {
                ivjlabelPPOI = new javax.swing.JLabel();
                ivjlabelPPOI.setName("labelPPOI");
                ivjlabelPPOI.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelPPOI.setText("Primary Point of Interest");
                ivjlabelPPOI.setBounds(17, 68, 143, 14);
                ivjlabelPPOI.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelPPOI;
    }

    /**
     * Return the labelQO property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelQO()
    {
        if (ivjlabelQO == null)
        {
            try
            {
                ivjlabelQO = new javax.swing.JLabel();
                ivjlabelQO.setName("labelQO");
                ivjlabelQO.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelQO.setText("* Non Breach Flow (QO in CFS)");
                ivjlabelQO.setBounds(13, 361, 226, 14);
                ivjlabelQO.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelQO;
    }

    /**
     * Return the labelRequiredFieldsNotice property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelRequiredFieldsNotice()
    {
        if (ivjlabelRequiredFieldsNotice == null)
        {
            try
            {
                ivjlabelRequiredFieldsNotice = new javax.swing.JLabel();
                ivjlabelRequiredFieldsNotice
                        .setName("labelRequiredFieldsNotice");
                ivjlabelRequiredFieldsNotice.setFont(new java.awt.Font("Arial",
                        1, 10));
                ivjlabelRequiredFieldsNotice
                        .setText("Fields with a (*) are required.   Also, either VOL or SA must be filled in.");
                ivjlabelRequiredFieldsNotice.setBounds(12, 464, 402, 14);
                ivjlabelRequiredFieldsNotice.setForeground(java.awt.Color.red);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelRequiredFieldsNotice;
    }

    /**
     * Return the labelRiverName property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelRiverName()
    {
        if (ivjlabelRiverName == null)
        {
            try
            {
                ivjlabelRiverName = new javax.swing.JLabel();
                ivjlabelRiverName.setName("labelRiverName");
                ivjlabelRiverName.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelRiverName.setText("* River Name");
                ivjlabelRiverName.setBounds(17, 42, 124, 14);
                ivjlabelRiverName.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelRiverName;
    }

    /**
     * Return the labelSA property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelSA()
    {
        if (ivjlabelSA == null)
        {
            try
            {
                ivjlabelSA = new javax.swing.JLabel();
                ivjlabelSA.setName("labelSA");
                ivjlabelSA.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelSA.setText("Surface Area of Reservoir (SA in ACRES)");
                ivjlabelSA.setBounds(13, 280, 226, 14);
                ivjlabelSA.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelSA;
    }

    /**
     * Return the labelScenario property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelScenario()
    {
        if (ivjlabelScenario == null)
        {
            try
            {
                ivjlabelScenario = new javax.swing.JLabel();
                ivjlabelScenario.setName("labelScenario");
                ivjlabelScenario.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelScenario.setText("* Scenario");
                ivjlabelScenario.setBounds(12, 145, 80, 14);
                ivjlabelScenario.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelScenario;
    }

    /**
     * Return the labelScenarioList property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelScenarioList()
    {
        if (ivjlabelScenarioList == null)
        {
            try
            {
                ivjlabelScenarioList = new javax.swing.JLabel();
                ivjlabelScenarioList.setName("labelScenarioList");
                ivjlabelScenarioList.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelScenarioList.setText("Scenario List:");
                ivjlabelScenarioList.setBounds(14, 12, 116, 14);
                ivjlabelScenarioList.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelScenarioList;
    }

    /**
     * Return the labelFloodFlow1 property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelSectionType()
    {
        if (ivjlabelSectionType == null)
        {
            try
            {
                ivjlabelSectionType = new javax.swing.JLabel();
                ivjlabelSectionType.setName("labelSectionType");
                ivjlabelSectionType.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelSectionType.setText("Cross Section Type");
                ivjlabelSectionType.setBounds(13, 97, 112, 14);
                ivjlabelSectionType.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelSectionType;
    }

    /**
     * Return the labelSource property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelSource()
    {
        if (ivjlabelSource == null)
        {
            try
            {
                ivjlabelSource = new javax.swing.JLabel();
                ivjlabelSource.setName("labelSource");
                ivjlabelSource.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelSource.setText("* Source");
                ivjlabelSource.setBounds(12, 119, 80, 14);
                ivjlabelSource.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelSource;
    }

    /**
     * Return the labelTFM property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelTFM()
    {
        if (ivjlabelTFM == null)
        {
            try
            {
                ivjlabelTFM = new javax.swing.JLabel();
                ivjlabelTFM.setName("labelTFM");
                ivjlabelTFM.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelTFM.setText("Time of Dam Failure (TFM in MINUTES)");
                ivjlabelTFM.setBounds(13, 333, 226, 14);
                ivjlabelTFM.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelTFM;
    }

    /**
     * Return the labelTypeOfDam property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelTypeOfDam()
    {
        if (ivjlabelTypeOfDam == null)
        {
            try
            {
                ivjlabelTypeOfDam = new javax.swing.JLabel();
                ivjlabelTypeOfDam.setName("labelTypeOfDam");
                ivjlabelTypeOfDam.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelTypeOfDam.setText("* Type Of Dam");
                ivjlabelTypeOfDam.setBounds(13, 172, 226, 14);
                ivjlabelTypeOfDam.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelTypeOfDam;
    }

    /**
     * Return the labelVolume property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelVolume()
    {
        if (ivjlabelVolume == null)
        {
            try
            {
                ivjlabelVolume = new javax.swing.JLabel();
                ivjlabelVolume.setName("labelVolume");
                ivjlabelVolume.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelVolume.setText("Volume of Reservoir (VOL in ACRE-FT)");
                ivjlabelVolume.setBounds(13, 253, 226, 14);
                ivjlabelVolume.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelVolume;
    }

    /**
     * Return the labelXSDistance property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelXSDistance()
    {
        if (ivjlabelXSDistance == null)
        {
            try
            {
                ivjlabelXSDistance = new javax.swing.JLabel();
                ivjlabelXSDistance.setName("labelXSDistance");
                ivjlabelXSDistance.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelXSDistance
                        .setText("Distance From Dam to Section (MILES)");
                ivjlabelXSDistance.setBounds(19, 154, 213, 14);
                ivjlabelXSDistance.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelXSDistance;
    }

    /**
     * Return the labelXSFloodDepth property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelXSFloodDepth()
    {
        if (ivjlabelXSFloodDepth == null)
        {
            try
            {
                ivjlabelXSFloodDepth = new javax.swing.JLabel();
                ivjlabelXSFloodDepth.setName("labelXSFloodDepth");
                ivjlabelXSFloodDepth.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelXSFloodDepth.setText("Flood Depth (FT)");
                ivjlabelXSFloodDepth.setBounds(20, 183, 132, 14);
                ivjlabelXSFloodDepth.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelXSFloodDepth;
    }

    /**
     * Return the labelFloodFlow property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelXSFloodFlow()
    {
        if (ivjlabelXSFloodFlow == null)
        {
            try
            {
                ivjlabelXSFloodFlow = new javax.swing.JLabel();
                ivjlabelXSFloodFlow.setName("labelXSFloodFlow");
                ivjlabelXSFloodFlow.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelXSFloodFlow.setText("Flow At Flood Stage (CFS)");
                ivjlabelXSFloodFlow.setBounds(19, 214, 201, 14);
                ivjlabelXSFloodFlow.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelXSFloodFlow;
    }

    /**
     * Return the labelXSLatitutde property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelXSLatitutde()
    {
        if (ivjlabelXSLatitutde == null)
        {
            try
            {
                ivjlabelXSLatitutde = new javax.swing.JLabel();
                ivjlabelXSLatitutde.setName("labelXSLatitutde");
                ivjlabelXSLatitutde.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelXSLatitutde.setText("Latitude");
                ivjlabelXSLatitutde.setBounds(20, 244, 58, 14);
                ivjlabelXSLatitutde.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelXSLatitutde;
    }

    /**
     * Return the labelXSList property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelXSList()
    {
        if (ivjlabelXSList == null)
        {
            try
            {
                ivjlabelXSList = new javax.swing.JLabel();
                ivjlabelXSList.setName("labelXSList");
                ivjlabelXSList.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelXSList.setText("Downstream Point List:");
                ivjlabelXSList.setBounds(13, 10, 162, 14);
                ivjlabelXSList.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelXSList;
    }

    /**
     * Return the labelXSList1 property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelXSList1()
    {
        if (ivjlabelXSList1 == null)
        {
            try
            {
                ivjlabelXSList1 = new javax.swing.JLabel();
                ivjlabelXSList1.setName("labelXSList1");
                ivjlabelXSList1.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelXSList1.setText("Cross Section List:");
                ivjlabelXSList1.setBounds(8, 9, 142, 14);
                ivjlabelXSList1.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelXSList1;
    }

    /**
     * Return the labelXSList11 property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelXSList11()
    {
        if (ivjlabelXSList11 == null)
        {
            try
            {
                ivjlabelXSList11 = new javax.swing.JLabel();
                ivjlabelXSList11.setName("labelXSList11");
                ivjlabelXSList11.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelXSList11.setText("Best Section Type: ");
                ivjlabelXSList11.setBounds(19, 300, 118, 14);
                ivjlabelXSList11.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelXSList11;
    }

    /**
     * Return the labelXSLongitude property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelXSLongitude()
    {
        if (ivjlabelXSLongitude == null)
        {
            try
            {
                ivjlabelXSLongitude = new javax.swing.JLabel();
                ivjlabelXSLongitude.setName("labelXSLongitude");
                ivjlabelXSLongitude.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelXSLongitude.setText("Longitude");
                ivjlabelXSLongitude.setBounds(19, 274, 66, 14);
                ivjlabelXSLongitude.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelXSLongitude;
    }

    /**
     * Return the labelXSName property value.
     * 
     * @return javax.swing.JLabel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JLabel getlabelXSName()
    {
        if (ivjlabelXSName == null)
        {
            try
            {
                ivjlabelXSName = new javax.swing.JLabel();
                ivjlabelXSName.setName("labelXSName");
                ivjlabelXSName.setFont(new java.awt.Font("Arial", 1, 10));
                ivjlabelXSName.setText("Downstream Point Name");
                ivjlabelXSName.setBounds(18, 125, 198, 14);
                ivjlabelXSName.setForeground(java.awt.Color.black);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlabelXSName;
    }

    /**
     * Return the listXSList property value.
     * 
     * @return javax.swing.JList
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JList getListDown()
    {
        if (ivjListDown == null)
        {
            try
            {
                ivjListDown = new javax.swing.JList();
                ivjListDown.setName("ListDown");
                ivjListDown.setBounds(0, 0, 186, 120);
                ivjListDown
                        .setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjListDown;
    }

    /**
     * Return the listModelDown property value.
     * 
     * @return javax.swing.DefaultListModel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.DefaultListModel getlistModelDown()
    {
        if (ivjlistModelDown == null)
        {
            try
            {
                ivjlistModelDown = new javax.swing.DefaultListModel();
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlistModelDown;
    }

    /**
     * Return the listModelScenario property value.
     * 
     * @return javax.swing.DefaultListModel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.DefaultListModel getlistModelScenario()
    {
        if (ivjlistModelScenario == null)
        {
            try
            {
                ivjlistModelScenario = new javax.swing.DefaultListModel();
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlistModelScenario;
    }

    /**
     * Return the listModelXS property value.
     * 
     * @return javax.swing.DefaultListModel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.DefaultListModel getlistModelXS()
    {
        if (ivjlistModelXS == null)
        {
            try
            {
                ivjlistModelXS = new javax.swing.DefaultListModel();
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjlistModelXS;
    }

    /**
     * Return the listScenarioList property value.
     * 
     * @return javax.swing.JList
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JList getListScenario()
    {
        if (ivjListScenario == null)
        {
            try
            {
                ivjListScenario = new javax.swing.JList();
                ivjListScenario.setName("ListScenario");
                ivjListScenario.setBounds(0, 0, 192, 120);
                ivjListScenario
                        .setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjListScenario;
    }

    /**
     * Return the listXSList1 property value.
     * 
     * @return javax.swing.JList
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JList getListXS()
    {
        if (ivjListXS == null)
        {
            try
            {
                ivjListXS = new javax.swing.JList();
                ivjListXS.setName("ListXS");
                ivjListXS.setBounds(0, 0, 177, 120);
                ivjListXS
                        .setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjListXS;
    }

    /**
     * Return the menuAbout property value.
     * 
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getmenuAbout()
    {
        if (ivjmenuAbout == null)
        {
            try
            {
                ivjmenuAbout = new javax.swing.JMenuItem();
                ivjmenuAbout.setName("menuAbout");
                ivjmenuAbout.setMnemonic('a');
                ivjmenuAbout.setText("About SMPDBK...");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuAbout;
    }

    /**
     * Return the menuClose property value.
     * 
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getmenuClose()
    {
        if (ivjmenuClose == null)
        {
            try
            {
                ivjmenuClose = new javax.swing.JMenuItem();
                ivjmenuClose.setName("menuClose");
                ivjmenuClose.setMnemonic('c');
                ivjmenuClose.setText("Close");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuClose;
    }

    /**
     * Return the menuDatabase property value.
     * 
     * @return javax.swing.JMenu
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenu getmenuDatabase()
    {
        if (ivjmenuDatabase == null)
        {
            try
            {
                ivjmenuDatabase = new javax.swing.JMenu();
                ivjmenuDatabase.setName("menuDatabase");
                ivjmenuDatabase.setMnemonic('d');
                ivjmenuDatabase.setText("Database");
                ivjmenuDatabase.add(getmenuUpdateDatabase());
                ivjmenuDatabase.add(getmenuClose());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuDatabase;
    }

    /**
     * Return the menuDisplayOutput property value.
     * 
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getmenuDisplayOutput()
    {
        if (ivjmenuDisplayOutput == null)
        {
            try
            {
                ivjmenuDisplayOutput = new javax.swing.JMenuItem();
                ivjmenuDisplayOutput.setName("menuDisplayOutput");
                ivjmenuDisplayOutput.setMnemonic('D');
                ivjmenuDisplayOutput.setText("Display SMPDBK Output");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuDisplayOutput;
    }

    /**
     * Return the menuEdit property value.
     * 
     * @return javax.swing.JMenu
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenu getmenuEdit()
    {
        if (ivjmenuEdit == null)
        {
            try
            {
                ivjmenuEdit = new javax.swing.JMenu();
                ivjmenuEdit.setName("menuEdit");
                ivjmenuEdit.setMnemonic('e');
                ivjmenuEdit.setText("Edit");
                ivjmenuEdit.add(getmenuEditText());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuEdit;
    }

    /**
     * Return the menuEditText property value.
     * 
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getmenuEditText()
    {
        if (ivjmenuEditText == null)
        {
            try
            {
                ivjmenuEditText = new javax.swing.JMenuItem();
                ivjmenuEditText.setName("menuEditText");
                ivjmenuEditText.setMnemonic('e');
                ivjmenuEditText.setText("Edit Text File");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuEditText;
    }

    /**
     * Return the menuFile property value.
     * 
     * @return javax.swing.JMenu
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenu getmenuFile()
    {
        if (ivjmenuFile == null)
        {
            try
            {
                ivjmenuFile = new javax.swing.JMenu();
                ivjmenuFile.setName("menuFile");
                ivjmenuFile.setMnemonic('f');
                ivjmenuFile.setText("File");
                ivjmenuFile.add(getmenuNew());
                ivjmenuFile.add(getmenuOpen());
                ivjmenuFile.add(getmenuSave());
                ivjmenuFile.add(getmenuSaveAs());
                ivjmenuFile.add(getJSeparator1());
                ivjmenuFile.add(getmenuImportFLDXS());
                ivjmenuFile.add(getmenuImportDAT());
                ivjmenuFile.add(getJSeparator2());
                ivjmenuFile.add(getmenuQuit());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuFile;
    }

    /**
     * Return the menuHelp property value.
     * 
     * @return javax.swing.JMenu
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenu getmenuHelp()
    {
        if (ivjmenuHelp == null)
        {
            try
            {
                ivjmenuHelp = new javax.swing.JMenu();
                ivjmenuHelp.setName("menuHelp");
                ivjmenuHelp.setMnemonic('h');
                ivjmenuHelp.setText("Help");
                ivjmenuHelp.add(getmenuAbout());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuHelp;
    }

    /**
     * Return the menuImportDAT property value.
     * 
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getmenuImportDAT()
    {
        if (ivjmenuImportDAT == null)
        {
            try
            {
                ivjmenuImportDAT = new javax.swing.JMenuItem();
                ivjmenuImportDAT.setName("menuImportDAT");
                ivjmenuImportDAT.setMnemonic('d');
                ivjmenuImportDAT.setText("Import SMPDBK input file");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuImportDAT;
    }

    /**
     * Return the menuImportFLDXS property value.
     * 
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getmenuImportFLDXS()
    {
        if (ivjmenuImportFLDXS == null)
        {
            try
            {
                ivjmenuImportFLDXS = new javax.swing.JMenuItem();
                ivjmenuImportFLDXS.setName("menuImportFLDXS");
                ivjmenuImportFLDXS.setMnemonic('X');
                ivjmenuImportFLDXS.setText("Import FLDXS Cross Sections");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuImportFLDXS;
    }

    /**
     * Return the menuNew property value.
     * 
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getmenuNew()
    {
        if (ivjmenuNew == null)
        {
            try
            {
                ivjmenuNew = new javax.swing.JMenuItem();
                ivjmenuNew.setName("menuNew");
                ivjmenuNew.setMnemonic('n');
                ivjmenuNew.setText("New");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuNew;
    }

    /**
     * Return the menuOpen property value.
     * 
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getmenuOpen()
    {
        if (ivjmenuOpen == null)
        {
            try
            {
                ivjmenuOpen = new javax.swing.JMenuItem();
                ivjmenuOpen.setName("menuOpen");
                ivjmenuOpen.setMnemonic('o');
                ivjmenuOpen.setText("Open...");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuOpen;
    }

    /**
     * Return the menuQuit property value.
     * 
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getmenuQuit()
    {
        if (ivjmenuQuit == null)
        {
            try
            {
                ivjmenuQuit = new javax.swing.JMenuItem();
                ivjmenuQuit.setName("menuQuit");
                ivjmenuQuit.setMnemonic('q');
                ivjmenuQuit.setText("Quit");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuQuit;
    }

    /**
     * Insert the method's description here. Creation date: (9/8/2003 9:10:37
     * AM)
     * 
     * @return javax.swing.JMenuItem
     */
    private JMenuItem getmenuRun_Scenario()
    {
        return null;
    }

    /**
     * Return the menuOutputManager property value.
     * 
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    public javax.swing.JMenuItem getmenuRun_SMPDBK()
    {
        if (ivjmenuRun_SMPDBK == null)
        {
            try
            {
                ivjmenuRun_SMPDBK = new javax.swing.JMenuItem();
                ivjmenuRun_SMPDBK.setName("menuRun_SMPDBK");
                ivjmenuRun_SMPDBK.setMnemonic('R');
                ivjmenuRun_SMPDBK.setText("Run SMPDBK");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuRun_SMPDBK;
    }

    /**
     * Return the menuSave property value.
     * 
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getmenuSave()
    {
        if (ivjmenuSave == null)
        {
            try
            {
                ivjmenuSave = new javax.swing.JMenuItem();
                ivjmenuSave.setName("menuSave");
                ivjmenuSave.setMnemonic('s');
                ivjmenuSave.setText("Save");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuSave;
    }

    /**
     * Return the menuSaveAs property value.
     * 
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getmenuSaveAs()
    {
        if (ivjmenuSaveAs == null)
        {
            try
            {
                ivjmenuSaveAs = new javax.swing.JMenuItem();
                ivjmenuSaveAs.setName("menuSaveAs");
                ivjmenuSaveAs.setMnemonic('a');
                ivjmenuSaveAs.setText("Save As...");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuSaveAs;
    }

    /**
     * Return the menuSMPDBK property value.
     * 
     * @return javax.swing.JMenu
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenu getmenuSMPDBK()
    {
        if (ivjmenuSMPDBK == null)
        {
            try
            {
                ivjmenuSMPDBK = new javax.swing.JMenu();
                ivjmenuSMPDBK.setName("menuSMPDBK");
                ivjmenuSMPDBK.setMnemonic('m');
                ivjmenuSMPDBK.setText("Model");
                ivjmenuSMPDBK.add(getmenuRun_SMPDBK());
                ivjmenuSMPDBK.add(getmenuDisplayOutput());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuSMPDBK;
    }

    /**
     * Return the menuUpdateDatabase property value.
     * 
     * @return javax.swing.JMenuItem
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuItem getmenuUpdateDatabase()
    {
        if (ivjmenuUpdateDatabase == null)
        {
            try
            {
                ivjmenuUpdateDatabase = new javax.swing.JMenuItem();
                ivjmenuUpdateDatabase.setName("menuUpdateDatabase");
                ivjmenuUpdateDatabase.setMnemonic('u');
                ivjmenuUpdateDatabase.setText("Update Database");
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjmenuUpdateDatabase;
    }

    /**
     * Return the ModelGUIJMenuBar property value.
     * 
     * @return javax.swing.JMenuBar
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JMenuBar getModelGUIJMenuBar()
    {
        if (ivjModelGUIJMenuBar == null)
        {
            try
            {
                ivjModelGUIJMenuBar = new javax.swing.JMenuBar();
                ivjModelGUIJMenuBar.setName("ModelGUIJMenuBar");
                ivjModelGUIJMenuBar.add(getmenuFile());
                ivjModelGUIJMenuBar.add(getmenuDatabase());
                ivjModelGUIJMenuBar.add(getmenuSMPDBK());
                ivjModelGUIJMenuBar.add(getmenuEdit());
                ivjModelGUIJMenuBar.add(getmenuHelp());
                // user code begin {1}
                if (bHasDatabase)
                    ivjModelGUIJMenuBar.remove(getmenuFile());
                else
                    ivjModelGUIJMenuBar.remove(getmenuDatabase());
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjModelGUIJMenuBar;
    }

    /**
     * Return the panelDamInfo property value.
     * 
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getpanelDamInfo()
    {
        if (ivjpanelDamInfo == null)
        {
            try
            {
                ivjpanelDamInfo = new javax.swing.JPanel();
                ivjpanelDamInfo.setName("panelDamInfo");
                ivjpanelDamInfo.setBorder(new EtchedBorder());
                ivjpanelDamInfo.setLayout(null);
                ivjpanelDamInfo.setBackground(new java.awt.Color(234, 223, 246));
                ivjpanelDamInfo.setBounds(1, 103, 420, 488);
                getpanelDamInfo().add(getlabelScenarioList(),
                        getlabelScenarioList().getName());
                getpanelDamInfo().add(getscrollScenarioList(),
                        getscrollScenarioList().getName());
                getpanelDamInfo().add(getlabelSource(),
                        getlabelSource().getName());
                getpanelDamInfo().add(getlabelScenario(),
                        getlabelScenario().getName());
                getpanelDamInfo().add(getlabelTypeOfDam(),
                        getlabelTypeOfDam().getName());
                getpanelDamInfo().add(getlabelHDE(), getlabelHDE().getName());
                getpanelDamInfo().add(getlabelBME(), getlabelBME().getName());
                getpanelDamInfo().add(getlabelVolume(),
                        getlabelVolume().getName());
                getpanelDamInfo().add(getlabelSA(), getlabelSA().getName());
                getpanelDamInfo().add(getlabelBW(), getlabelBW().getName());
                getpanelDamInfo().add(getlabelTFM(), getlabelTFM().getName());
                getpanelDamInfo().add(getlabelQO(), getlabelQO().getName());
                getpanelDamInfo().add(getlabelDISTTN(),
                        getlabelDISTTN().getName());
                getpanelDamInfo().add(getlabelCMS(), getlabelCMS().getName());
                getpanelDamInfo().add(getlabelRequiredFieldsNotice(),
                        getlabelRequiredFieldsNotice().getName());
                getpanelDamInfo().add(gettextSource(),
                        gettextSource().getName());
                getpanelDamInfo().add(getcomboScenario(),
                        getcomboScenario().getName());
                getpanelDamInfo().add(getcomboTypeOfDam(),
                        getcomboTypeOfDam().getName());
                getpanelDamInfo().add(gettextHDE(), gettextHDE().getName());
                getpanelDamInfo().add(gettextBME(), gettextBME().getName());
                getpanelDamInfo().add(gettextVOL(), gettextVOL().getName());
                getpanelDamInfo().add(gettextSA(), gettextSA().getName());
                getpanelDamInfo().add(gettextBW(), gettextBW().getName());
                getpanelDamInfo().add(gettextTFM(), gettextTFM().getName());
                getpanelDamInfo().add(gettextQO(), gettextQO().getName());
                getpanelDamInfo().add(gettextDISTTN(),
                        gettextDISTTN().getName());
                getpanelDamInfo().add(gettextCMS(), gettextCMS().getName());
                getpanelDamInfo().add(getlabelDBUG(), getlabelDBUG().getName());
                getpanelDamInfo().add(getcomboDBUG(), getcomboDBUG().getName());
                // user code begin {1}
                // user code end
            } 
            catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjpanelDamInfo;
    }

    /**
     * Return the panelXSInfo property value.
     * 
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getpanelDownstreamInfo()
    {
        if (ivjpanelDownstreamInfo == null)
        {
            try
            {
                ivjpanelDownstreamInfo = new javax.swing.JPanel();
                ivjpanelDownstreamInfo.setName("panelDownstreamInfo");
                ivjpanelDownstreamInfo
                        .setBorder(new EtchedBorder());
                ivjpanelDownstreamInfo.setLayout(null);
                ivjpanelDownstreamInfo.setBackground(new java.awt.Color(236,
                        243, 255));
                ivjpanelDownstreamInfo.setBounds(422, 4, 395, 590);
                getpanelDownstreamInfo().add(getlabelXSList(),
                        getlabelXSList().getName());
                getpanelDownstreamInfo().add(getscrollXSList(),
                        getscrollXSList().getName());
                getpanelDownstreamInfo().add(getlabelXSName(),
                        getlabelXSName().getName());
                getpanelDownstreamInfo().add(getlabelXSDistance(),
                        getlabelXSDistance().getName());
                getpanelDownstreamInfo().add(getlabelXSFloodDepth(),
                        getlabelXSFloodDepth().getName());
                getpanelDownstreamInfo().add(getlabelXSFloodFlow(),
                        getlabelXSFloodFlow().getName());
                getpanelDownstreamInfo().add(getlabelXSLatitutde(),
                        getlabelXSLatitutde().getName());
                getpanelDownstreamInfo().add(getlabelXSLongitude(),
                        getlabelXSLongitude().getName());
                getpanelDownstreamInfo().add(gettextName(),
                        gettextName().getName());
                getpanelDownstreamInfo().add(gettextDistance(),
                        gettextDistance().getName());
                getpanelDownstreamInfo().add(gettextFloodDepth(),
                        gettextFloodDepth().getName());
                getpanelDownstreamInfo().add(gettextFloodFlow(),
                        gettextFloodFlow().getName());
                getpanelDownstreamInfo().add(gettextLatitude(),
                        gettextLatitude().getName());
                getpanelDownstreamInfo().add(gettextLongitude(),
                        gettextLongitude().getName());
                getpanelDownstreamInfo().add(getpanelXS(),
                        getpanelXS().getName());
                getpanelDownstreamInfo().add(getlabelXSList11(),
                        getlabelXSList11().getName());
                getpanelDownstreamInfo().add(getcomboBestXS(),
                        getcomboBestXS().getName());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjpanelDownstreamInfo;
    }

    /**
     * Return the panelDamInfo1 property value.
     * 
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getpanelNameInfo()
    {
        if (ivjpanelNameInfo == null)
        {
            try
            {
                ivjpanelNameInfo = new javax.swing.JPanel();
                ivjpanelNameInfo.setName("panelNameInfo");
                ivjpanelNameInfo
                        .setBorder(new EtchedBorder());
                ivjpanelNameInfo.setLayout(null);
                ivjpanelNameInfo
                        .setBackground(new java.awt.Color(227, 228, 235));
                ivjpanelNameInfo.setBounds(1, 5, 419, 97);
                getpanelNameInfo().add(getlabelDamName(),
                        getlabelDamName().getName());
                getpanelNameInfo().add(getlabelRiverName(),
                        getlabelRiverName().getName());
                getpanelNameInfo()
                        .add(getlabelPPOI(), getlabelPPOI().getName());
                getpanelNameInfo().add(gettextDamName(),
                        gettextDamName().getName());
                getpanelNameInfo().add(gettextRiverName(),
                        gettextRiverName().getName());
                getpanelNameInfo().add(gettextPPOI(), gettextPPOI().getName());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjpanelNameInfo;
    }

    /**
     * Return the JPanel1 property value.
     * 
     * @return javax.swing.JPanel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JPanel getpanelXS()
    {
        if (ivjpanelXS == null)
        {
            try
            {
                ivjpanelXS = new javax.swing.JPanel();
                ivjpanelXS.setName("panelXS");
                ivjpanelXS.setBorder(new EtchedBorder());
                ivjpanelXS.setLayout(null);
                ivjpanelXS.setBackground(new java.awt.Color(164, 218, 204));
                ivjpanelXS.setBounds(18, 329, 359, 257);
                getpanelXS()
                        .add(getlabelXSList1(), getlabelXSList1().getName());
                getpanelXS().add(getscrollXSList1(),
                        getscrollXSList1().getName());
                getpanelXS().add(getlabelSectionType(),
                        getlabelSectionType().getName());
                getpanelXS().add(getcomboXSType(), getcomboXSType().getName());
                getpanelXS()
                        .add(getscrollXSData(), getscrollXSData().getName());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjpanelXS;
    }

    /**
     * Return the scrollScenarioList property value.
     * 
     * @return javax.swing.JScrollPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JScrollPane getscrollScenarioList()
    {
        if (ivjscrollScenarioList == null)
        {
            try
            {
                ivjscrollScenarioList = new javax.swing.JScrollPane();
                ivjscrollScenarioList.setName("scrollScenarioList");
                ivjscrollScenarioList.setBounds(16, 34, 377, 77);
                getscrollScenarioList().setViewportView(getListScenario());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjscrollScenarioList;
    }

    /**
     * Return the scrollXSData property value.
     * 
     * @return javax.swing.JScrollPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JScrollPane getscrollXSData()
    {
        if (ivjscrollXSData == null)
        {
            try
            {
                ivjscrollXSData = new javax.swing.JScrollPane();
                ivjscrollXSData.setName("scrollXSData");
                ivjscrollXSData.setBounds(7, 124, 342, 130);
                ivjscrollXSData.setEnabled(false);
                getscrollXSData().setViewportView(gettableXSData());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjscrollXSData;
    }

    /**
     * Return the scrollXSList property value.
     * 
     * @return javax.swing.JScrollPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JScrollPane getscrollXSList()
    {
        if (ivjscrollXSList == null)
        {
            try
            {
                ivjscrollXSList = new javax.swing.JScrollPane();
                ivjscrollXSList.setName("scrollXSList");
                ivjscrollXSList.setBounds(15, 32, 364, 74);
                getscrollXSList().setViewportView(getListDown());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjscrollXSList;
    }

    /**
     * Return the scrollXSList1 property value.
     * 
     * @return javax.swing.JScrollPane
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JScrollPane getscrollXSList1()
    {
        if (ivjscrollXSList1 == null)
        {
            try
            {
                ivjscrollXSList1 = new javax.swing.JScrollPane();
                ivjscrollXSList1.setName("scrollXSList1");
                ivjscrollXSList1.setBounds(10, 28, 337, 57);
                getscrollXSList1().setViewportView(getListXS());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjscrollXSList1;
    }

    /**
     * Return the tableModelXSGeom property value.
     * 
     * @return javax.swing.table.DefaultTableModel
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.table.DefaultTableModel gettableModelXSGeom()
    {
        if (ivjtableModelXSGeom == null)
        {
            try
            {
                ivjtableModelXSGeom = new javax.swing.table.DefaultTableModel();
                // user code begin {1}
                ivjtableModelXSGeom = new javax.swing.table.DefaultTableModel(
                        8, 4);
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtableModelXSGeom;
    }

    /**
     * Return the tableXSData property value.
     * 
     * @return javax.swing.JTable
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTable gettableXSData()
    {
        if (ivjtableXSData == null)
        {
            try
            {
                ivjtableXSData = new javax.swing.JTable();
                ivjtableXSData.setName("tableXSData");
                getscrollXSData().setColumnHeaderView(
                        ivjtableXSData.getTableHeader());
                getscrollXSData().getViewport().setBackingStoreEnabled(true);
                ivjtableXSData.setBounds(0, 0, 332, 136);
                ivjtableXSData.setEnabled(false);
                ivjtableXSData.setAutoCreateColumnsFromModel(false);
                ivjtableXSData.addColumn(getcolumnElevation());
                ivjtableXSData.addColumn(getcolumnBS());
                ivjtableXSData.addColumn(getcolumnBSS());
                ivjtableXSData.addColumn(getcolumnMannN());
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtableXSData;
    }

    /**
     * Return the textBME property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextBME()
    {
        if (ivjtextBME == null)
        {
            try
            {
                ivjtextBME = new javax.swing.JTextField();
                ivjtextBME.setName("textBME");
                ivjtextBME.setBounds(269, 222, 125, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextBME;
    }

    /**
     * Return the textBW property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextBW()
    {
        if (ivjtextBW == null)
        {
            try
            {
                ivjtextBW = new javax.swing.JTextField();
                ivjtextBW.setName("textBW");
                ivjtextBW.setBounds(269, 303, 125, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextBW;
    }

    /**
     * Return the textCMS property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextCMS()
    {
        if (ivjtextCMS == null)
        {
            try
            {
                ivjtextCMS = new javax.swing.JTextField();
                ivjtextCMS.setName("textCMS");
                ivjtextCMS.setBounds(269, 411, 125, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextCMS;
    }

    /**
     * Return the textDamName property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextDamName()
    {
        if (ivjtextDamName == null)
        {
            try
            {
                ivjtextDamName = new javax.swing.JTextField();
                ivjtextDamName.setName("textDamName");
                ivjtextDamName.setDocument(new MaxLengthDocument(40));
                ivjtextDamName.setBounds(164, 12, 229, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextDamName;
    }

    /**
     * Return the textXSDistance property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextDistance()
    {
        if (ivjtextDistance == null)
        {
            try
            {
                ivjtextDistance = new javax.swing.JTextField();
                ivjtextDistance.setName("textDistance");
                ivjtextDistance.setBounds(234, 152, 143, 20);
                ivjtextDistance.setEnabled(false);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextDistance;
    }

    /**
     * Return the textDISTTN property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextDISTTN()
    {
        if (ivjtextDISTTN == null)
        {
            try
            {
                ivjtextDISTTN = new javax.swing.JTextField();
                ivjtextDISTTN.setName("textDISTTN");
                ivjtextDISTTN.setBounds(269, 384, 125, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextDISTTN;
    }

    /**
     * Return the textXSFloodDepth property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextFloodDepth()
    {
        if (ivjtextFloodDepth == null)
        {
            try
            {
                ivjtextFloodDepth = new javax.swing.JTextField();
                ivjtextFloodDepth.setName("textFloodDepth");
                ivjtextFloodDepth.setBounds(234, 180, 143, 20);
                ivjtextFloodDepth.setEnabled(false);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextFloodDepth;
    }

    /**
     * Return the textFloodFlow property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextFloodFlow()
    {
        if (ivjtextFloodFlow == null)
        {
            try
            {
                ivjtextFloodFlow = new javax.swing.JTextField();
                ivjtextFloodFlow.setName("textFloodFlow");
                ivjtextFloodFlow.setBounds(234, 210, 143, 20);
                ivjtextFloodFlow.setEnabled(false);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextFloodFlow;
    }

    /**
     * Return the textHDE property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextHDE()
    {
        if (ivjtextHDE == null)
        {
            try
            {
                ivjtextHDE = new javax.swing.JTextField();
                ivjtextHDE.setName("textHDE");
                ivjtextHDE.setBounds(269, 195, 125, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextHDE;
    }

    /**
     * Return the textXSLatitude property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextLatitude()
    {
        if (ivjtextLatitude == null)
        {
            try
            {
                ivjtextLatitude = new javax.swing.JTextField();
                ivjtextLatitude.setName("textLatitude");
                ivjtextLatitude.setBounds(234, 241, 143, 20);
                ivjtextLatitude.setEnabled(false);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextLatitude;
    }

    /**
     * Return the textXSLongitude property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextLongitude()
    {
        if (ivjtextLongitude == null)
        {
            try
            {
                ivjtextLongitude = new javax.swing.JTextField();
                ivjtextLongitude.setName("textLongitude");
                ivjtextLongitude.setBounds(234, 270, 143, 20);
                ivjtextLongitude.setEnabled(false);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextLongitude;
    }

    /**
     * Return the textXSName property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextName()
    {
        if (ivjtextName == null)
        {
            try
            {
                ivjtextName = new javax.swing.JTextField();
                ivjtextName.setName("textName");
                ivjtextName.setBounds(234, 122, 143, 20);
                ivjtextName.setEnabled(false);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextName;
    }

    /**
     * Return the textPPOI property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextPPOI()
    {
        if (ivjtextPPOI == null)
        {
            try
            {
                ivjtextPPOI = new javax.swing.JTextField();
                ivjtextPPOI.setName("textPPOI");
                ivjtextPPOI.setDocument(new MaxLengthDocument(25));
                ivjtextPPOI.setBounds(164, 68, 229, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextPPOI;
    }

    /**
     * Return the textQO property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextQO()
    {
        if (ivjtextQO == null)
        {
            try
            {
                ivjtextQO = new javax.swing.JTextField();
                ivjtextQO.setName("textQO");
                ivjtextQO.setBounds(269, 357, 125, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextQO;
    }

    /**
     * Return the textRiverName property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextRiverName()
    {
        if (ivjtextRiverName == null)
        {
            try
            {
                ivjtextRiverName = new javax.swing.JTextField();
                ivjtextRiverName.setName("textRiverName");
                ivjtextRiverName.setDocument(new MaxLengthDocument(40));
                ivjtextRiverName.setBounds(164, 39, 229, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextRiverName;
    }

    /**
     * Return the textSA property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextSA()
    {
        if (ivjtextSA == null)
        {
            try
            {
                ivjtextSA = new javax.swing.JTextField();
                ivjtextSA.setName("textSA");
                ivjtextSA.setBounds(269, 276, 125, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextSA;
    }

    /**
     * Return the textSource property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextSource()
    {
        if (ivjtextSource == null)
        {
            try
            {
                ivjtextSource = new javax.swing.JTextField();
                ivjtextSource.setName("textSource");
                ivjtextSource.setDocument(new MaxLengthDocument(40));
                ivjtextSource.setBounds(269, 120, 125, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextSource;
    }

    /**
     * Return the textTFM property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextTFM()
    {
        if (ivjtextTFM == null)
        {
            try
            {
                ivjtextTFM = new javax.swing.JTextField();
                ivjtextTFM.setName("textTFM");
                ivjtextTFM.setBounds(269, 330, 125, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextTFM;
    }

    /**
     * Return the textVOL property value.
     * 
     * @return javax.swing.JTextField
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private javax.swing.JTextField gettextVOL()
    {
        if (ivjtextVOL == null)
        {
            try
            {
                ivjtextVOL = new javax.swing.JTextField();
                ivjtextVOL.setName("textVOL");
                ivjtextVOL.setBounds(269, 249, 125, 20);
                // user code begin {1}
                // user code end
            } catch (java.lang.Throwable ivjExc)
            {
                // user code begin {2}
                // user code end
                handleException(ivjExc);
            }
        }
        return ivjtextVOL;
    }

    /**
     * For DownstreamPoints:
     * 
     * Handle INSERT Key
     * 
     * Handle DELETE or <- (backspace)
     */
    private void handleDownListKey(java.awt.event.KeyEvent keyEvent)
    {

        if (getListDown().getSelectedIndex() == -1 || bHandlingEvent)
        {
            return;
        }

        if (keyEvent.getKeyCode() == KeyEvent.VK_BACK_SPACE
                || keyEvent.getKeyCode() == KeyEvent.VK_DELETE)
        {
            bHandlingEvent = true;

            int oldSelection = getListDown().getSelectedIndex();

            getListDown().clearSelection();

            // *** move selected DownstreamPoint to deleted ArrayList
            DownstreamPoint delDP = (DownstreamPoint) currentData.downstream
                    .get(oldSelection);
            currentData.deletedDownstream.add(delDP);

            // *** now remove from primary ArrayList
            currentData.downstream.remove(oldSelection);

            updateDownstreamList();

            clearDownstreamFields();
            enableDownstreamEditors(false);

            bHandlingEvent = false;
        }

        if (keyEvent.getKeyCode() == KeyEvent.VK_INSERT)
        {
            bHandlingEvent = true;

            int oldSelection = getListDown().getSelectedIndex() + 1;

            getListDown().clearSelection();

            createNewDownstreamPoint(oldSelection);

            clearDownstreamFields();

            enableDownstreamEditors(false);

            bHandlingEvent = false;
        }

        return;
    }

    /**
     * invoke updateDownstreamFields if focus lost on key fields
     */
    private void handleDownstreamKeyLostFocus()
    {

        if (bHandlingEvent)
        {
            return;
        }

        bHandlingEvent = true;

        if (nDownstreamEditing != -1)
        {
            updateDownstreamFields(true);
        }

        bHandlingEvent = false;
        return;
    }

    /**
     * This method is similar to handleScenarioListSelection and
     * handleXSListSelection
     */
    private void handleDownstreamListSelection( ListSelectionEvent listSelectionEvent)
    {

        // only process event if the selection is final and no other event is
        // being handled
        if (listSelectionEvent.getValueIsAdjusting() || bHandlingEvent)
        {
            return;
        }

        bHandlingEvent = true;

        int selectedIndex = getListDown().getSelectedIndex();
        _selectedDownstreamPoint = selectedIndex;

        // save downstream data if data is currently being edited -
        // if the data is invalid, stop handling event
        if (!updateDownstreamFields(true))
        {
            JOptionPane
                    .showMessageDialog(
                            this,
                            "The downstream point name and distance pair must be unique.",
                            "Error", JOptionPane.ERROR_MESSAGE);
            bHandlingEvent = false;
            getListDown().setSelectedIndex(nDownstreamEditing);
            _selectedDownstreamPoint = nDownstreamEditing;
            return;
        }

        // if the selection is being cleared, clear the downstream editors
        if (selectedIndex == -1)
        {
            clearDownstreamFields();
            enableDownstreamEditors(false);
            bHandlingEvent = false;
            return;
        }

        // if "Create a new downstream point..." was clicked, create a new
        // downstream point and select it for editing
        // if (_DEBUG) System.out.println("Selected D/S Station/point Index: " +
        // getListDown().getSelectedIndex());
        if (selectedIndex == getlistModelDown().getSize() - 1)
        {

            // the SMPDBK model allows a maximum of 50 downstream locations
            if (currentData.downstream.size() >= 50)
            {
                Toolkit.getDefaultToolkit().beep();
                bHandlingEvent = false;
                getListDown().setSelectedIndex(nDownstreamEditing);
                _selectedDownstreamPoint = nDownstreamEditing;
                return;
            }

            createNewDownstreamPoint(selectedIndex);
        }

        // load data for selected downstream point into GUI
        updateDownstreamFields(false);
        enableDownstreamEditors(true);

        bHandlingEvent = false; // *** selecting XSList will fire new event !
        // *** so turn off bHandlingEvent first !

        // if there is an XS chosen, select it !
        if (_selectedXSType != -1)
        {
            getListXS().clearSelection();
            getListXS().setSelectedIndex(_selectedXSType);
        }

    }

    /**
     * Called whenever the part throws an exception.
     * 
     * @param exception
     *            java.lang.Throwable
     */
    private void handleException(java.lang.Throwable exception)
    {

        /* Uncomment the following lines to print uncaught exceptions to stdout */
        System.out
                .println("--------- UNCAUGHT EXCEPTION in ModelGUI ---------");
        exception.printStackTrace(System.out);
    }

    /**
     * Comment
     */
    private void handleMenuAbout()
    {
        // JOptionPane.showMessageDialog(this,"Dambreak Analysis v1.0 (build
        // 2)\nCraig Austin","About Dambreak
        // Analysis",JOptionPane.INFORMATION_MESSAGE);
        JOptionPane
                .showMessageDialog(
                        this,
                        "Dambreak Analysis v1.0 (Build 5.3 - 06/01/2004)\nby National Weather Service, \n - Office of Hydrologic Development",
                        "About Dambreak Analysis",
                        JOptionPane.INFORMATION_MESSAGE);
        return;
    }

    /**
     * Comment
     */
    private void handleMenuClose()
    {
        handleWindowClosing();
        return;
    }

    /**
     * Insert the method's description here. Creation date: (10/3/2003 3:40:14
     * PM)
     */
    public void handleMenuDisplayOutput()
    {

        JFileChooser fileChooser2 = new JFileChooser(strLastOpenPath);
        fileChooser2.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fileChooser2.setFileFilter(new DamInputFileFilter(
                DamInputFileFilter.OUTPUT));
        fileChooser2.setDialogTitle("Select Dambreak Analysis Output File");
        int result = fileChooser2.showOpenDialog(this);

        // user clicked Cancel button on dialog
        if (result != JFileChooser.APPROVE_OPTION)
            return;

        File fileName = fileChooser2.getSelectedFile();

        if (fileName == null || fileName.getName().equals(""))
            JOptionPane.showMessageDialog(this, "Invalid File Name",
                    "Invalid File Name", JOptionPane.ERROR_MESSAGE);
        else
        {
            // set our last opened path
            strLastOpenPath = fileName.getPath();

            if (_DEBUG)
                System.out.print("in handleMenuDisplayOutput Opening "
                        + fileName.getAbsolutePath() + "...\n");

            strOpenedFilename = fileName.getAbsolutePath();

            if (strOpenedFilename == null)
                JOptionPane.showMessageDialog(this,
                        "Error opening dambreak output file.\n"
                                + fileName.getAbsolutePath(), "Error",
                        JOptionPane.ERROR_MESSAGE);
            else
            {
                ModelOutput outFile = ModelOutput.readOUT(fileName
                        .getAbsolutePath());
                if (outFile == null)
                    JOptionPane.showMessageDialog(this,
                            "Attempt to open not SMPDBK formatted file.\n"
                                    + fileName.getAbsolutePath(), "Error",
                            JOptionPane.ERROR_MESSAGE);
                else
                {
                    // instantiate Analysis Data and copy data from ModelOutput
                    // change currentData to outputOnlyData

                    outputOnlyData = new AnalysisData();
                    currentData.importModelOutput(outFile);

                    // generateOutput(); // not needed !

                    OutputManager outMan = new OutputManager(this, currentData);
                    outMan.setTitle("Output Manager - "
                            + fileName.getAbsolutePath());

                    System.out.println("Finished displaying out file.");

                    return;
                }
            }
        }
        return;
    }

    /**
     * This method needed for editing file via GUI. Creation date: (9/26/2003
     * 1:53:13 PM)
     */
    private void handleMenuEditText() throws Exception
    {
        String osName = System.getProperty("os.name");
        String lowerCaseName = osName.toLowerCase();
        Runtime rt = Runtime.getRuntime();
        String dbaEditor = "";
        String runThis = "";

        JFileChooser fileChooser1 = new JFileChooser(strLastOpenPath);
        fileChooser1.setDialogTitle("Select the File to edit");
        int result1 = fileChooser1.showOpenDialog(this);

        // user clicked Cancel button on dialog
        if (result1 != JFileChooser.APPROVE_OPTION)
            return;

        File fileName1 = fileChooser1.getSelectedFile();

        if (fileName1 == null || fileName1.getName().equals(""))
        {
            JOptionPane.showMessageDialog(this, "Invalid File Name",
                    "Invalid File Name", JOptionPane.ERROR_MESSAGE);
        } else
        {
            runThis = fileName1.getAbsolutePath();
        }

        try
        {
            dbaEditor = PropertyReader.getProperty("damcrest.editor");
            if (lowerCaseName.indexOf("windows") > -1)
            {
                // System.out.println("Editing a file with " + dbaEditor + " \""
                // + runThis + "\"");
                rt.exec(dbaEditor + " \"" + runThis + "\"");
            } else if (lowerCaseName.indexOf("linux") > -1)
            {
                // System.out.println("Editing a file with " + dbaEditor + " " +
                // runThis);
                rt.exec(dbaEditor + " " + runThis);
            }
        } catch (IOException e)
        {
            // Problem editing the file
            e.printStackTrace();
            JOptionPane.showMessageDialog(this, "Cannot edit the file",
                    "Cannot edit the file", JOptionPane.ERROR_MESSAGE);

        }

        return;
    }

    /**
     * This method reads information from a *.DAT file. assumed to be in SMPDBK
     * format
     */
    private void handleMenuImportFromDAT()
    {

        handleMenuNew();

        JFileChooser fileChooser = new JFileChooser(strLastOpenPath);
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fileChooser.setFileFilter(new DamInputFileFilter(
                DamInputFileFilter.DAT_INPUT));
        fileChooser.setDialogTitle("Select SMPDBK Input File");
        int result = fileChooser.showOpenDialog(this);

        // user clicked Cancel button on dialog
        if (result != JFileChooser.APPROVE_OPTION)
            return;

        File fileName = fileChooser.getSelectedFile();

        if (fileName == null || fileName.getName().equals(""))
            JOptionPane.showMessageDialog(this, "Invalid File Name",
                    "Invalid File Name", JOptionPane.ERROR_MESSAGE);
        else
        {
            // set our last opened path
            strLastOpenPath = fileName.getPath();

            try
            {
                if (_DEBUG)
                    System.out.print("in handleMenuImportFromDAT opening "
                            + fileName.getAbsolutePath() + "...\n");
                if (!currentData.importDAT(fileName.getAbsolutePath()))
                {
                    JOptionPane.showMessageDialog(this,
                            "Error importing -- wrong data or file type used for DAT file!\n"
                                    + fileName.getAbsolutePath(), "Error",
                            JOptionPane.ERROR_MESSAGE);
                    return;
                }
                if (_DEBUG)
                    System.out.println("Finished import .DAT");

                // update GUI to show newly imported data
                updateNameFields(false);
                updateScenarioList();
                updateDownstreamList();
                updateXSList();
                bHasDataToSave = true;
            } catch (Exception e)
            {
                e.printStackTrace();
                JOptionPane.showMessageDialog(this,
                        "Error importing -- bad DAT file!", "Error",
                        JOptionPane.ERROR_MESSAGE);
                return;
            }
        }
    }

    /**
     * Read Data from .xs file format (created by FLDXS)
     */
    private boolean handleMenuImportFromFLDXS()
    {

        handleMenuNew();

        JFileChooser fileChooser = new JFileChooser(strLastOpenPath);
        fileChooser.setDialogTitle("Select FldXS Output File");
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fileChooser.setFileFilter(new DamInputFileFilter(
                DamInputFileFilter.TXT_INPUT));
        fileChooser.setDialogTitle("Select FLDXS cross section to import");
        int result = fileChooser.showOpenDialog(this);

        // user clicked Cancel button on dialog
        if (result != JFileChooser.APPROVE_OPTION)
            return false;

        // user sets path
        File fileName = fileChooser.getSelectedFile();
        strLastOpenPath = fileName.getPath();

        try
        {
            if (_DEBUG)
                System.out.print("in  handleMenuImportFromFLDXS opening "
                        + fileName.getAbsolutePath() + "...\n");

            if (!currentData.importFLDXS(fileName.getAbsolutePath()))
                JOptionPane.showMessageDialog(this,
                        "Error importing -- wrong data or file type used for FLDXS file!\n"
                                + fileName.getAbsolutePath(), "Error",
                        JOptionPane.ERROR_MESSAGE);

            if (_DEBUG)
                System.out.println("Finished import FLDXS.");

            // update GUI to show newly imported data
            updateDownstreamList();
            updateXSList();

            bHasDataToSave = true;
            bChangedData = false;

        } catch (Exception e)
        {
            e.printStackTrace();
            JOptionPane.showMessageDialog(this,
                    "Error importing -- bad FLDXS file!", "Error",
                    JOptionPane.ERROR_MESSAGE);
            return false;
        }
        return true;
    }

    /**
     * User has hit "New" menu option on "File" or "Database" menu
     */
    private void handleMenuNew()
    {

        // Ask the user if they want to save before clearing the GUI.
        // If the save fails, don't clear.

        if (bHasDataToSave && bChangedData)
        {
            saveGUI();
            if (!checkForSaveChanges())
                return;
        }

        bHandlingEvent = false;
        currentData = new AnalysisData();
        strOpenedFilename = null;
        updateGUI();
        bHasDataToSave = false;
        return;

    }

    /**
     * opens .DAM file format
     */
    private boolean handleMenuOpen()
    {

        handleMenuNew(); // *** clear GUI

        JFileChooser fileChooser = new JFileChooser(strLastOpenPath);
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fileChooser.setFileFilter(new DamInputFileFilter(
                DamInputFileFilter.DAM_INPUT));
        fileChooser.setDialogTitle("Select Dambreak Analysis Input File");
        int result = fileChooser.showOpenDialog(this);

        // user clicked Cancel button on dialog
        if (result != JFileChooser.APPROVE_OPTION)
            return false;

        File fileName = fileChooser.getSelectedFile();

        if (fileName == null || fileName.getName().equals(""))
            JOptionPane.showMessageDialog(this, "Invalid File Name",
                    "Invalid File Name", JOptionPane.ERROR_MESSAGE);
        else
        {
            // set our last opened path
            strLastOpenPath = fileName.getPath();

            if (_DEBUG)
                System.out.print("In handleMenuOpen Opening "
                        + fileName.getAbsolutePath() + "...\n");

            AnalysisData newData;
            if ((newData = AnalysisData.readDAM(fileName.getAbsolutePath())) == null)
                JOptionPane.showMessageDialog(this,
                        "Error opening dambreak input DAM file.\n"
                                + fileName.getAbsolutePath(), "Error",
                        JOptionPane.ERROR_MESSAGE);
            else
                currentData = newData;

            strOpenedFilename = fileName.getAbsolutePath();

            if (_DEBUG)
                System.out.println("Finished opening dam file.");

            // update GUI to show newly imported data
            updateNameFields(false);
            updateScenarioList();
            updateDownstreamList();
            updateXSList();
            bHasDataToSave = true;
            bChangedData = false;
        }
        return true;
    }

    /**
     * Comment
     */
    private void handleMenuQuit()
    {
        handleWindowClosing();
        return;
    }

    /**
     * Handle when "Run SMPDBK" menu item chosen
     */
    public void handleMenuRun_SMPDBK()
    {

        // save any data currently being edited in the GUI back to the
        // AnalysisData structure
        saveGUI();

        // update all input data in case any input data was changed to their
        // default values
        updateNameFields(false);
        if (nScenarioEditing != -1)
            updateScenarioFields(false);

        // verify that the model can be run on this input
        String error = currentData.verifyInput();

        if (error != null)
        {
            JOptionPane.showMessageDialog(this, error,
                    "Errors in Data - Can Not Run SMPDBK Model",
                    JOptionPane.ERROR_MESSAGE);
            return;
        }

        // update all the output structures
        if (generateOutput() == true)
        {
            // to get the prerun result from the database
            String oError = currentData.verifyOutput(); // what's done with this
                                                        // string ?

            OutputManager outMan = new OutputManager(this, currentData);
            outMan.setTitle("Output Manager - " + "(most recent model run)");
            //outMan.show()
        } else
        {
            JOptionPane.showMessageDialog(this, "Output Generation Failed",
                    "SMPDBK Model Run Was Unsuccessful",
                    JOptionPane.ERROR_MESSAGE);
        }
        return;
    }

    /**
     * If a file is open already, this replaces it with updated data. If there
     * is no open file, "Save As..." is automatically called.
     */
    private boolean handleMenuSave()
    {
        // save any data currently being edited in the GUI back to the
        // AnalysisData structure
        saveGUI();

        if (strOpenedFilename == null)
            return handleMenuSaveAs();
        else
            return AnalysisData.writeDAM(currentData, strOpenedFilename);
    }

    /**
     * NOTE:
     */
    private boolean handleMenuSaveAs()
    {
        File fileName;

        // save any data currently being edited in the GUI back to the
        // AnalysisData structure
        saveGUI();

        JFileChooser fileChooser = new JFileChooser(strLastOpenPath);
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fileChooser.setDialogTitle("Select File Name To Save To");
        fileChooser.setFileFilter(new DamInputFileFilter(
                DamInputFileFilter.DAM_INPUT));
        int result = fileChooser.showSaveDialog(this);
        FileWriter output;

        // user clicked Cancel button on dialog
        if (result != JFileChooser.APPROVE_OPTION)
            return false;

        fileName = fileChooser.getSelectedFile();
        if (fileName == null || fileName.getName().equals(""))
        {
            JOptionPane.showMessageDialog(this, "Invalid File Name",
                    "Invalid File Name", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        // set the 'last file opened' path
        strLastOpenPath = fileName.getPath();

        String strFileName = fileName.getAbsolutePath();
        if (!strFileName.toLowerCase().endsWith(".dam"))
            strFileName = strFileName.concat(".DAM");

        if (!AnalysisData.writeDAM(currentData, strFileName))
        {
            JOptionPane.showMessageDialog(this, "Error writing file.", "Error",
                    JOptionPane.ERROR_MESSAGE);
            return false;
        }

        strOpenedFilename = fileName.getAbsolutePath();
        bChangedData = false;

        return true;
    }

    /**
     * This writes all the data in the ModelGUI AnalysisData structure back to
     * the tables of the DAMCAT database.
     */
    private boolean handleMenuUpdateDatabase(boolean bConfirmMessage)
    {

        String methodName = "*** handleMenuUpdateDatabase";
        // if (_DEBUG) System.out.println(methodName);

        int selectedIndex = getListScenario().getSelectedIndex();

        // save any data currently being edited in the GUI back into the
        // AnalysisData structure

        saveGUI();

        int confirmOption;

        if (bConfirmMessage)
            confirmOption = JOptionPane.showConfirmDialog(this,
                    "All data in the DAMCAT database entry for dam \""
                            + strWorkingDamNIDID
                            + "\" will be replaced.\nContinue?", "Confirm",
                    JOptionPane.YES_NO_OPTION);
        else
        {
            confirmOption = JOptionPane.YES_OPTION;
        }

        if (confirmOption == JOptionPane.NO_OPTION)
            // return false; // *** perhaps this should be true !
            return true;

        // First, verify that the model can be run on this input
        String error = currentData.verifyInput();

        // update all input data in case any input data was changed to their
        // default values
        updateNameFields(false);

        if (nScenarioEditing != -1)
            updateScenarioFields(false);

        if (error != null)
        {
            JOptionPane.showMessageDialog(this, error,
                    "Could Not Run SMPDBK Model", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        // update all the output structures
        if (generateOutput() == true)
        {
            try
            {
                dbAccess.updateModelData(strWorkingDamNIDID, currentData);
                JOptionPane.showMessageDialog(this,
                        "Successfully updated database entry \""
                                + strWorkingDamNIDID.trim() + "\".", "Success",
                        JOptionPane.INFORMATION_MESSAGE);

            } catch (Exception e)
            {

                JOptionPane.showMessageDialog(this,
                        "Could not update DAMCAT entry!", "Error",
                        JOptionPane.ERROR_MESSAGE);
                e.printStackTrace();
                return false;
            }
        } else
        {
            JOptionPane.showMessageDialog(this,
                    "The Database was not updated!",
                    "SMPDBK Model Run Was Unsuccessful",
                    JOptionPane.ERROR_MESSAGE);
            return false;
        }

        bChangedData = false;

        return true;
    }

    /**
     * Comment
     */
    private void handleScenarioKeyFocusLost()
    {

        if (bHandlingEvent)
        {
            return;
        }

        bHandlingEvent = true;

        if (nScenarioEditing != -1)
        {
            updateScenarioFields(true);
        }

        bHandlingEvent = false;
        return;

    }

    /**
     * For Input Scenarios:
     * 
     * Handle INSERT Key
     * 
     * Handle DELETE or <- (backspace)
     */
    private void handleScenarioListKey(java.awt.event.KeyEvent keyEvent)
    {

        if (getListScenario().getSelectedIndex() == -1 || bHandlingEvent)
        {
            return;
        }

        if (keyEvent.getKeyCode() == KeyEvent.VK_BACK_SPACE
                || keyEvent.getKeyCode() == KeyEvent.VK_DELETE)
        {
            bHandlingEvent = true;

            int oldSelection = getListScenario().getSelectedIndex();

            getListScenario().clearSelection();

            // *** move selected ModelScenario to deleted ArrayList
            ModelScenario delMS = (ModelScenario) currentData.scenarios
                    .get(oldSelection);
            currentData.deletedScenarios.add(delMS);

            // *** now remove from primary ArrayList			
            currentData.scenarios.remove(oldSelection);

            updateScenarioList();

            clearScenarioFields();

            enableScenarioEditors(false);

            bHandlingEvent = false;
        }

        if (keyEvent.getKeyCode() == KeyEvent.VK_INSERT)
        {
            bHandlingEvent = true;

            int newSelection = getListScenario().getSelectedIndex() + 1;

            getListScenario().clearSelection();

            ModelScenario newScenario = new ModelScenario();

            currentData.scenarios.add(newSelection, newScenario);

            updateScenarioList();

            getListScenario().setSelectedIndex(newSelection);
            _selectedScenario = newSelection;

            clearScenarioFields();

            updateScenarioFields(false);

            enableScenarioEditors(true);

            bHandlingEvent = false;

        }
        return;
    }

    /**
     * Handle Selection of New List
     */
    private void handleScenarioListSelection(ListSelectionEvent listSelectionEvent)
    {

        // only process event if the selection is final and no other event is being handled
        if (listSelectionEvent.getValueIsAdjusting() || bHandlingEvent)
        {
            return;
        }

        bHandlingEvent = true;

        int selectedIndex = getListScenario().getSelectedIndex();

        // save scenario data if data is currently being edited - 
        //
        // *** if the data is invalid, stop handling event !	
        if (!updateScenarioFields(true))
        {
            JOptionPane.showMessageDialog(this,
                    "The scenario name and source pair must be unique.",
                    "Error", JOptionPane.ERROR_MESSAGE);
            bHandlingEvent = false;
            getListScenario().setSelectedIndex(nScenarioEditing);
            _selectedScenario = nScenarioEditing;
            return;
        }

        // if the selection is being cleared, clear the scenario editors
        if (selectedIndex == -1)
        {
            clearScenarioFields();
            enableScenarioEditors(false);
            bHandlingEvent = false;
            return;
        }

        // if "Highlight me to create a new scenario..." was clicked, create a new scenario and select it for editing
        if (selectedIndex == getlistModelScenario().getSize() - 1)
        {

            createNewScenario();
            int idx = currentData.scenarios.size() - 1;
            getListScenario().setSelectedIndex(idx);
            _selectedScenario = (idx);

        }

        // load data for selected scenario into GUI

        updateScenarioFields(false);

        enableScenarioEditors(true);

        bHandlingEvent = false;
    }

    /**
     * Close Window and Exit to Op Sys.
     */
    private void handleWindowClosing()
    {

        if (bHasDataToSave || bChangedData) // was &&
        {
            saveGUI();
            if (!checkForSaveChanges())
                return;
        }

        dispose();

        if ((!bHasDatabase) || (Launcher.closeFlag))
        {
            System.exit(0);
        }
        return;

    }

    /**
     * Lost Focus Event fired for which fields ???
     */
    private void handleXSKeyLostFocus()
    {

        if (bHandlingEvent)
        {
            return;
        }

        bHandlingEvent = true;

        if (nXSEditing != -1)
        {
            updateXSFields(true);
        }

        bHandlingEvent = false;
        return;
    }

    /**
     * Comment
     */
    private void handleXSListClick(java.awt.event.MouseEvent mouseEvent)
    {

        // only handle double clicks
        if (mouseEvent.getClickCount() < 2
                || nDownstreamEditing == -1
                || getListXS().getSelectedIndex() == -1
                || getListXS().getSelectedIndex() == getlistModelXS().getSize() - 1)
        {
            return;
        }

        bHandlingEvent = true;

        // *** should this be here ???
        // DownstreamPoint downEditing = (DownstreamPoint)currentData.downstream.get(nDownstreamEditing);
        // downEditing.bestXS = getListXS().getSelectedIndex();

        updateXSList();

        bHandlingEvent = false;
        return;
    }

    /**
     * For XS Types
     * 
     * Handle INSERT Key 
     *
     * Handle DELETE or <- (backspace) 
     */
    private void handleXSListKey(java.awt.event.KeyEvent keyEvent)
    {

        if (getListXS().getSelectedIndex() == -1 || bHandlingEvent)
        {
            return;
        }

        if (keyEvent.getKeyCode() == KeyEvent.VK_BACK_SPACE
                || keyEvent.getKeyCode() == KeyEvent.VK_DELETE)
        {
            bHandlingEvent = true;

            int oldSelection = getListXS().getSelectedIndex();

            DownstreamPoint down = (DownstreamPoint) currentData.downstream
                    .get(nDownstreamEditing);

            // if this is the only XS, do not let the user remove it  
            // *** Why ????
            if (down.xsections.size() == 1)
            {
                Toolkit.getDefaultToolkit().beep();
                bHandlingEvent = false;
                return;
            }

            // *** move selected XS to deleted ArrayList
            SectionGeometry delXS = (SectionGeometry) down.xsections
                    .get(oldSelection);
            down.deletedXsections.add(delXS);

            // *** now remove from primary ArrayList		
            down.xsections.remove(oldSelection);

            if (down.bestXS == oldSelection && down.xsections.size() > 0)
                down.bestXS = 0;
            else if (down.bestXS == oldSelection)
                down.bestXS = -1;

            getListXS().clearSelection();
            updateXSList();

            clearXSFields();
            enableXSEditors(false);

            bHandlingEvent = false;
        }

        if (keyEvent.getKeyCode() == KeyEvent.VK_INSERT)
        {
            if (_DEBUG)
                System.out.println("Insert XS key pressed: "
                        + getListXS().getSelectedIndex());

            bHandlingEvent = true;

            int oldSelection = getListXS().getSelectedIndex();

            DownstreamPoint down = (DownstreamPoint) currentData.downstream
                    .get(nDownstreamEditing);

            getListXS().clearSelection();

            SectionGeometry newSection = new SectionGeometry();

            down.xsections.add(newSection);

            updateXSList();

            clearXSFields();

            enableXSEditors(false);

            bHandlingEvent = false;

        }

        return;
    }

    /**
     * Comment
     */
    private void handleXSListSelection(ListSelectionEvent listSelectionEvent)
    {

        // only process event if the selection is final 
        if (listSelectionEvent.getValueIsAdjusting())
        {
            return;
        }
        // and no other event is being handled
        if (bHandlingEvent)
        {
            return;
        }

        bHandlingEvent = true;

        int selectedIndex = getListXS().getSelectedIndex();

        // save section data if data is currently being edited - if the data is invalid, stop handling event
        if (!updateXSFields(true))
        {
            JOptionPane.showMessageDialog(this,
                    "The cross section type must be unique.", "Error",
                    JOptionPane.ERROR_MESSAGE);
            bHandlingEvent = false;
            getListXS().setSelectedIndex(nXSEditing);
            _selectedXSType = nXSEditing;
            return;
        }

        // updateXSFields(true);		// *** this only needs done once !

        // if the selection is being cleared, clear the section editors
        if (selectedIndex == -1)
        {
            clearXSFields();
            enableXSEditors(false);
            bHandlingEvent = false;
            return;
        }

        // if "Create a section..." was clicked, create a new section and select it for editing
        // if (_DEBUG) System.out.println("Selected Cross Section Type Index: " + getListXS().getSelectedIndex());

        if (selectedIndex == getlistModelXS().getSize() - 1)
        {
            createNewXS(nDownstreamEditing);
        }

        // load data for selected section into GUI
        updateXSFields(false);

        enableXSEditors(true);

        bHandlingEvent = false;
    }

    /**
     * Initializes the combo boxes for the restricted fields (enumerated types).
     * Creation date: (7/29/2003 9:41:54 AM)
     */
    private void initComboBoxes()
    {
        //DownstreamPoint selectedPoint = (DownstreamPoint)currentData.downstream.get(selectedIndex);

        int count = 0; //to count scenarios where source starts with '#'

        String[] damTypes = EnumeratedTypes
                .getEnglishChoices(EnumeratedTypes.FIELD_DAMTYPE);
        for (int i = 0; i < damTypes.length; i++)
            getcomboTypeOfDam().addItem(damTypes[i]);

        String[] scenarios = EnumeratedTypes
                .getEnglishChoices(EnumeratedTypes.FIELD_SCENARIO);
        for (int i = 0; i < scenarios.length; i++)
            getcomboScenario().addItem(scenarios[i]);

        String[] xsTypes = EnumeratedTypes
                .getEnglishChoices(EnumeratedTypes.FIELD_XSTYPE);
        for (int i = 0; i < xsTypes.length; i++)
            getcomboXSType().addItem(xsTypes[i]);

        String[] dbugTypes = EnumeratedTypes
                .getEnglishChoices(EnumeratedTypes.FIELD_DBUG);
        for (int i = 0; i < dbugTypes.length; i++)
            getcomboDBUG().addItem(dbugTypes[i]);

        //initialize the Best Xsec Type combobox to an empty string	
        getcomboBestXS().addItem("");
    }

    /**
     * Initializes connections
     * @exception java.lang.Exception The exception description.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initConnections() throws java.lang.Exception
    {
        // user code begin {1}
        // user code end
        getListScenario().addListSelectionListener(
                new javax.swing.event.ListSelectionListener()
                {
                    public void valueChanged(
                            javax.swing.event.ListSelectionEvent e)
                    {
                        connEtoC1(e);
                    };
                });
        getListDown().addListSelectionListener(
                new javax.swing.event.ListSelectionListener()
                {
                    public void valueChanged(
                            javax.swing.event.ListSelectionEvent e)
                    {
                        connEtoC2(e);
                    };
                });
        getListXS().addKeyListener(new java.awt.event.KeyAdapter()
        {
            public void keyPressed(java.awt.event.KeyEvent e)
            {
                connEtoC3(e);
            };
        });
        getmenuQuit().addActionListener(new java.awt.event.ActionListener()
        {
            public void actionPerformed(java.awt.event.ActionEvent e)
            {
                connEtoC4(e);
            };
        });
        getListScenario().addKeyListener(new java.awt.event.KeyAdapter()
        {
            public void keyPressed(java.awt.event.KeyEvent e)
            {
                connEtoC5(e);
            };
        });
        getmenuNew().addActionListener(new java.awt.event.ActionListener()
        {
            public void actionPerformed(java.awt.event.ActionEvent e)
            {
                connEtoC8(e);
            };
        });
        getcomboScenario().addActionListener(
                new java.awt.event.ActionListener()
                {
                    public void actionPerformed(java.awt.event.ActionEvent e)
                    {
                        connEtoC6(e);
                    };
                });
        gettextSource().addFocusListener(new java.awt.event.FocusAdapter()
        {
            public void focusLost(java.awt.event.FocusEvent e)
            {
                connEtoC7(e);
            };
        });
        getListDown().addKeyListener(new java.awt.event.KeyAdapter()
        {
            public void keyPressed(java.awt.event.KeyEvent e)
            {
                connEtoC9(e);
            };
        });
        gettextName().addFocusListener(new java.awt.event.FocusAdapter()
        {
            public void focusLost(java.awt.event.FocusEvent e)
            {
                connEtoC10(e);
            };
        });
        gettextDistance().addFocusListener(new java.awt.event.FocusAdapter()
        {
            public void focusLost(java.awt.event.FocusEvent e)
            {
                connEtoC11(e);
            };
        });
        getListXS().addListSelectionListener(
                new javax.swing.event.ListSelectionListener()
                {
                    public void valueChanged(
                            javax.swing.event.ListSelectionEvent e)
                    {
                        connEtoC12(e);
                    };
                });
        getcomboXSType().addActionListener(new java.awt.event.ActionListener()
        {
            public void actionPerformed(java.awt.event.ActionEvent e)
            {
                connEtoC13(e);
            };
        });
        getListXS().addMouseListener(new java.awt.event.MouseAdapter()
        {
            public void mouseClicked(java.awt.event.MouseEvent e)
            {
                connEtoC14(e);
            };
        });
        this.addWindowListener(new java.awt.event.WindowAdapter()
        {
            public void windowClosing(java.awt.event.WindowEvent e)
            {
                connEtoC15(e);
            };
        });
        getmenuClose().addActionListener(new java.awt.event.ActionListener()
        {
            public void actionPerformed(java.awt.event.ActionEvent e)
            {
                connEtoC16(e);
            };
        });
        getmenuAbout().addActionListener(new java.awt.event.ActionListener()
        {
            public void actionPerformed(java.awt.event.ActionEvent e)
            {
                connEtoC18(e);
            };
        });
        getmenuImportDAT().addActionListener(
                new java.awt.event.ActionListener()
                {
                    public void actionPerformed(java.awt.event.ActionEvent e)
                    {
                        connEtoC19(e);
                    };
                });
        getmenuImportFLDXS().addActionListener(
                new java.awt.event.ActionListener()
                {
                    public void actionPerformed(java.awt.event.ActionEvent e)
                    {
                        connEtoC20(e);
                    };
                });
        getmenuUpdateDatabase().addActionListener(
                new java.awt.event.ActionListener()
                {
                    public void actionPerformed(java.awt.event.ActionEvent e)
                    {
                        connEtoC21(e);
                    };
                });
        getmenuSave().addActionListener(new java.awt.event.ActionListener()
        {
            public void actionPerformed(java.awt.event.ActionEvent e)
            {
                connEtoC22(e);
            };
        });
        getmenuOpen().addActionListener(new java.awt.event.ActionListener()
        {
            public void actionPerformed(java.awt.event.ActionEvent e)
            {
                connEtoC23(e);
            };
        });
        getmenuSaveAs().addActionListener(new java.awt.event.ActionListener()
        {
            public void actionPerformed(java.awt.event.ActionEvent e)
            {
                connEtoC24(e);
            };
        });
        getmenuEditText().addActionListener(new java.awt.event.ActionListener()
        {
            public void actionPerformed(java.awt.event.ActionEvent e)
            {
                connEtoC25(e);
            };
        });
        getmenuDisplayOutput().addActionListener(
                new java.awt.event.ActionListener()
                {
                    public void actionPerformed(java.awt.event.ActionEvent e)
                    {
                        connEtoC26(e);
                    };
                });
        getmenuRun_SMPDBK().addActionListener(
                new java.awt.event.ActionListener()
                {
                    public void actionPerformed(java.awt.event.ActionEvent e)
                    {
                        connEtoC27(e);
                    };
                });
        getcomboBestXS().addActionListener(new java.awt.event.ActionListener()
        {
            public void actionPerformed(java.awt.event.ActionEvent e)
            {
                connEtoC17();
            };
        });
        connPtoP1SetTarget();
        connPtoP2SetTarget();
        connPtoP3SetTarget();
        connPtoP4SetTarget();
    }

    /**
     * Insert the method's description here.
     * Creation date: (7/29/2003 10:39:10 AM)
     */
    private void initDocuments()
    {
        gettextHDE().setDocument(new NumbersOnlyDocument());
        gettextBME().setDocument(new NumbersOnlyDocument());
        gettextVOL().setDocument(new NumbersOnlyDocument());
        gettextTFM().setDocument(new NumbersOnlyDocument());
        gettextSA().setDocument(new NumbersOnlyDocument());
        gettextQO().setDocument(new NumbersOnlyDocument());
        gettextBW().setDocument(new NumbersOnlyDocument());
        gettextDISTTN().setDocument(new NumbersOnlyDocument());
        gettextCMS().setDocument(new NumbersOnlyDocument());

        gettextDistance().setDocument(new NumbersOnlyDocument());
        gettextFloodDepth().setDocument(new NumbersOnlyDocument());
        gettextFloodFlow().setDocument(new NumbersOnlyDocument());
        gettextLatitude().setDocument(new NumbersOnlyDocument());
        gettextLongitude().setDocument(new NumbersOnlyDocument());

        gettextDamName().setDocument(new MaxLengthDocument(40));
        gettextRiverName().setDocument(new MaxLengthDocument(40));
        gettextPPOI().setDocument(new MaxLengthDocument(25));
    }

    /**
     * Initialize the class.
     */
    /* WARNING: THIS METHOD WILL BE REGENERATED. */
    private void initialize()
    {
        try
        {
            // user code begin {1}
            // user code end
            setName("ModelGUI");
            setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
            setJMenuBar(getModelGUIJMenuBar());
            setResizable(false);
            setSize(817, 612);
            setTitle("Simplified Dambreak Model (SMPDBK)");
            setContentPane(getJFrameContentPane());
            initConnections();
        } catch (java.lang.Throwable ivjExc)
        {
            handleException(ivjExc);
        }
        // user code begin {2}

        initComboBoxes();
        initDocuments();

        enableScenarioEditors(false);
        updateScenarioList();

        enableDownstreamEditors(false);
        updateDownstreamList();

        enableXSEditors(false);
        updateXSList();

        updateNameFields(false);

        bChangedData = false;

        // user code end
    }

    /**
     * main entrypoint - starts the part when it is run as an application
     * @param args java.lang.String[]
     */
    public static void main(java.lang.String[] args)
    {
        try
        {
            ModelGUI aModelGUI;
            aModelGUI = new ModelGUI(false, null, null, null);
            aModelGUI.addWindowListener(new java.awt.event.WindowAdapter()
            {
                public void windowClosing(java.awt.event.WindowEvent e)
                {
                    System.exit(0);
                };
            });
//            aModelGUI.show();
            aModelGUI.setVisible( true );
            java.awt.Insets insets = aModelGUI.getInsets();
            aModelGUI.setSize(
                    aModelGUI.getWidth() + insets.left + insets.right,
                    aModelGUI.getHeight() + insets.top + insets.bottom);
            aModelGUI.setVisible(true);
        } catch (Throwable exception)
        {
            System.err
                    .println("Exception occurred in main() of javax.swing.JFrame");
            exception.printStackTrace(System.out);
        }
    }

    /**
     * Save any data currently being edited in the GUI back to the AnalysisData structure
     *
     * Creation date: (8/1/2003 5:19:19 PM)
     */
    private void saveGUI()
    {

        updateNameFields(true);

        if (nScenarioEditing != -1)
        {
            updateScenarioFields(true);
        }

        if (nDownstreamEditing != -1)
        {
            updateDownstreamFields(true);
        }

        if (nXSEditing != -1)
        {
            updateXSFields(true);
        }

    }

    /**
     * Insert the method's description here.
     * Creation date: (12/19/2003 11:38:55 AM)
     */
    public void setBestXSectionsCombo()
    {

        String methodName = "*** In ModelGUI.setBestXSSectionsCombo";
        // if (_DEBUG) System.out.println(methodName);

        int selectedIndex = getListDown().getSelectedIndex();

        DownstreamPoint selectedPoint = (DownstreamPoint) currentData.downstream
                .get(selectedIndex);
        nDownstreamEditing = selectedIndex;

        if (selectedPoint.xsecBestType == null)
            selectedPoint.xsecBestType = "";
        /*
         if (_DEBUG)
         {
         System.out.println("xsecBestType " +":" + selectedPoint.xsecBestType +":");
         }
         */

        ComboBoxModel model = getcomboBestXS().getModel();
        for (int i = 0; i < model.getSize(); i++)
        {
            String bestStr = (String) model.getElementAt(i);
            if (bestStr == null)
                bestStr = "";
            // if (_DEBUG) System.out.println("Each element: " + i + " :" + bestStr + ":");
            if (bestStr.equals(selectedPoint.xsecBestType))
            {
                // if (_DEBUG) System.out.println("xsec_types are equal: " + bestStr + "=" + selectedPoint.xsecBestType);
                getcomboBestXS().setSelectedIndex(i);
                int realSelectedIndex = getcomboBestXS().getSelectedIndex();
                // if (_DEBUG) System.out.println ("realSelectedIndex: " + realSelectedIndex);
                getcomboBestXS().repaint();
                break;
            }
        }
    }

    /**
     * Insert the method's description here.
     * Creation date: (7/30/2003 10:26:59 AM)
     */
    private void stopTableEditing()
    {
        try
        {
            int column = gettableXSData().getEditingColumn();
            if (column > -1)
            {
                TableCellEditor cellEditor = gettableXSData().getColumnModel()
                        .getColumn(column).getCellEditor();
                if (cellEditor == null)
                {
                    cellEditor = gettableXSData().getDefaultEditor(
                            gettableXSData().getColumnClass(column));
                }
                if (cellEditor != null)
                {
                    cellEditor.stopCellEditing();
                }
            }
        } catch (Exception e)
        {
        }
    }

    private void trace()
    {
        try
        {
            throw (new Exception("just tracing , not a real error"));
        } catch (Exception e)
        {
            e.printStackTrace();
        }
    }

    /**
     * Synchronizes the GUI and the data structures backing them up.
     *
     * If the input parameter is true, the data in the GUI is
     * saved to the data structures; 
     *
     * If the input parameter is false, the data in the GUI is 
     * replaced by the data in the structures.
     *
     * Creation date: (7/29/2003 10:20:53 AM)
     * @param bSaveFields boolean
     */
    private boolean updateDownstreamFields(boolean bSaveFields)
    {

        String methodName = "*** updateDownstreamFields";
        //if (_DEBUG) System.out.println(methodName);

        int selectedDS = getListDown().getSelectedIndex();

        if (selectedDS == -1
                || (selectedDS == getlistModelDown().getSize() - 1 && nDownstreamEditing == -1))
        {
            return true;
        }

        if (bSaveFields == true && nDownstreamEditing != -1) // Move data from GUI to Logical Data Structure
        // (bSaveFields == true && nDownstreamEditing != -1 && !(nDownstreamEditing > currentData.downstream.size()))
        {
            DownstreamPoint editedPoint = (DownstreamPoint) currentData.downstream
                    .get(nDownstreamEditing);

            // check to see if keys (data listed in DownstreamPoint List) have changed, if so, update the list
            boolean bKeysEdited = false;
            if (!editedPoint.name.equals(gettextName().getText())
                    || editedPoint.distanceToSection != NumFormat
                            .toFloat(gettextDistance().getText()))
            {
                bKeysEdited = true;

                // are the new keys unique?
                boolean bUniqueKeys = true;

                for (int i = 0; i < currentData.downstream.size(); i++)
                {
                    if (i == nDownstreamEditing)
                        continue;
                    DownstreamPoint d = (DownstreamPoint) currentData.downstream
                            .get(i);
                    if (gettextName().getText().equals(d.name)
                            && NumFormat.toFloat(gettextDistance().getText()) == d.distanceToSection)
                        bUniqueKeys = false;
                }
                if (!bUniqueKeys)
                    return false;
            }

            // *** test to see if anything was changed in the GUI 	
            String testString = gettextName().getText().trim();
            if (!editedPoint.name.equalsIgnoreCase(testString))
            {
                editedPoint.name = testString;
                editedPoint.changeFlag = 1;
                bChangedData = true;
            }
            if (editedPoint.distanceToSection != NumFormat
                    .toFloat(gettextDistance().getText()))
            {
                editedPoint.distanceToSection = NumFormat
                        .toFloat(gettextDistance().getText());
                editedPoint.changeFlag = 1;
                bChangedData = true;
            }
            if (editedPoint.floodDepth != NumFormat.toFloat(gettextFloodDepth()
                    .getText()))
            {
                editedPoint.floodDepth = NumFormat.toFloat(gettextFloodDepth()
                        .getText());
                editedPoint.changeFlag = 1;
                bChangedData = true;
            }
            if (editedPoint.floodFlow != NumFormat.toFloat(gettextFloodFlow()
                    .getText()))
            {
                editedPoint.floodFlow = NumFormat.toFloat(gettextFloodFlow()
                        .getText());
                editedPoint.changeFlag = 1;
                bChangedData = true;
            }
            if (editedPoint.latitude != NumFormat.toFloat(gettextLatitude()
                    .getText()))
            {
                editedPoint.latitude = NumFormat.toFloat(gettextLatitude()
                        .getText());
                editedPoint.changeFlag = 1;
                bChangedData = true;
            }
            if (editedPoint.longitude != NumFormat.toFloat(gettextLongitude()
                    .getText()))
            {
                editedPoint.longitude = NumFormat.toFloat(gettextLongitude()
                        .getText());
                editedPoint.changeFlag = 1;
                bChangedData = true;
            }
            int bestIndex = getcomboBestXS().getSelectedIndex();
            if (bestIndex > -1)
            {
                String bestString = (String) getcomboBestXS().getItemAt(
                        bestIndex);
                if (!editedPoint.xsecBestType.equalsIgnoreCase(bestString))
                {
                    editedPoint.xsecBestType = bestString;
                    editedPoint.changeFlag = 1;
                    bChangedData = true;
                }
            }

            // *** set best index
            for (int x = 0; x < editedPoint.xsections.size(); x++)
            {
                SectionGeometry sg = (SectionGeometry) editedPoint.xsections
                        .get(x);
                if (sg.getXSType().equalsIgnoreCase(editedPoint.xsecBestType))
                {
                    editedPoint.bestXS = x;
                    break;
                }
            }

            // *** save any section being edited ???
            updateXSFields(true);

            // update the downstream list if the keys have been edited
            if (bKeysEdited)
                updateDownstreamList();

        } else if (bSaveFields == false)
        { // *** Move data from Logical Data Structure to GUI 

            // *** first save associated XS fields in case anything changed there and wasn't recorded	
            updateXSFields(true);
            clearXSFields();

            DownstreamPoint selectedPoint = (DownstreamPoint) currentData.downstream
                    .get(selectedDS);
            nDownstreamEditing = selectedDS;

            gettextName().setText(selectedPoint.name);
            gettextDistance().setText(
                    NumFormat.format(selectedPoint.distanceToSection, 4));
            gettextFloodDepth().setText(
                    NumFormat.format(selectedPoint.floodDepth, 4));
            gettextFloodFlow().setText(
                    NumFormat.format(selectedPoint.floodFlow, 4));
            gettextLatitude().setText(
                    NumFormat.format(selectedPoint.latitude, 4));
            gettextLongitude().setText(
                    NumFormat.format(selectedPoint.longitude, 4));

            // clear combo box
            getcomboBestXS().setSelectedIndex(-1);
            getcomboBestXS().removeAllItems();

            for (int i = 0; i < selectedPoint.xsections.size(); i++)
            {
                SectionGeometry sg = (SectionGeometry) selectedPoint.xsections
                        .get(i);
                String xst = sg.getXSType();

                // add to combo box			
                getcomboBestXS().addItem(xst);
            }

            setBestXSectionsCombo();

            // refresh the cross section type list
            getListXS().clearSelection();
            updateXSList();
            enableXSEditors(false);
        }
        return true;
    }

    /**
     * Synchronizes the DownstreamPoint data structures with the DefaultListModel.
     * Preserves the selected downstream point.
     * Creation date: (7/29/2003 9:32:06 AM)
     */
    private void updateDownstreamList()
    {

        int oldSelection = getListDown().getSelectedIndex();

        int interestPoint = 0;

        getlistModelDown().clear();

        DownstreamPoint dpEditing = null;
        float distanceEditing = 1000000.0f; // A million miles	

        // *** Save info for DownstreamPoint whereever nDownstreamEditing is focused

        if (nDownstreamEditing > -1)
        {

            dpEditing = (DownstreamPoint) currentData.downstream
                    .get(nDownstreamEditing);
            distanceEditing = dpEditing.distanceToSection;

        }

        currentData.sortDownstreamByDistance();

        for (int i = 0; i < currentData.downstream.size(); i++)
        {
            DownstreamPoint down = (DownstreamPoint) currentData.downstream
                    .get(i);
            if (distanceEditing == down.distanceToSection)
            {
                // System.out.println("*** Changing nDownstreamEditing from " + nDownstreamEditing + " to " + i);
                nDownstreamEditing = i;
            }
            getlistModelDown().addElement(
                    down.name + " - Distance: "
                            + NumFormat.format(down.distanceToSection, 4));
            // *** determine which DownstreamPoint is of most interest
            if (down.distanceToSection == currentData.pointOfInterestDistance)
                interestPoint = i;
        }

        getlistModelDown().addElement(
                "Highlight me to append a new cross section location");

        if (oldSelection == -1 && getlistModelDown().size() > 1) // test if more than the "Highlight me " entry
            oldSelection = interestPoint; // if so, set to point of interest, or first one

        if (nDownstreamEditing > -1) // however, if we were updating one of the points
            oldSelection = nDownstreamEditing; // make that one the choice

        getListDown().setSelectedIndex(oldSelection);
        _selectedDownstreamPoint = oldSelection;
    }

    /**
     * Clear Fields In GUI 
     * Creation date: (7/29/2003 11:55:11 AM)
     */
    private void updateGUI()
    {

        updateNameFields(false);

        getListScenario().clearSelection();
        updateScenarioList();
        clearScenarioFields();
        enableScenarioEditors(false);

        getListDown().clearSelection();
        updateDownstreamList();
        clearDownstreamFields();
        enableDownstreamEditors(false);
    }

    /**
     * Transfer data between ModelGUI and AnalysisData structure
     *
     * If the parameter is true, the data in the GUI is
     * saved to the data structure; if the parameter is false,
     * the data in the GUI is replaced by the data in the
     * structure.
     *
     * Creation date: (8/1/2003 9:45:06 AM)
     * @param bSaveFields boolean
     */
    private void updateNameFields(boolean bSaveFields)
    {

        //String methodName = "*** updateNameFields";
        //if (_DEBUG) System.out.println(methodName);

        if (bSaveFields)
        {
            if (currentData.damName != gettextDamName().getText())
            {
                currentData.damName = gettextDamName().getText();
                currentData.changeFlag = 1;
                bChangedData = true;
            }
            if (currentData.riverName != gettextRiverName().getText())
            {
                currentData.riverName = gettextRiverName().getText();
                currentData.changeFlag = 1;
                bChangedData = true;
            }
            if (currentData.pointOfInterestName != gettextPPOI().getText())
            {
                currentData.pointOfInterestName = gettextPPOI().getText();
                currentData.changeFlag = 1;
                bChangedData = true;
            }
        } else
        {
            gettextDamName().setText(currentData.damName);
            gettextRiverName().setText(currentData.riverName);
            gettextPPOI().setText(currentData.pointOfInterestName);
        }
    }

    /**
     * Synchronizes the editors and the data backing them up.
     *
     * If the parameter is true, the data in the editors is saved to the data structure; 
     *
     * if the parameter is false, data in the editors is replaced by data in the structure.
     *
     * Creation date: (7/29/2003 10:20:53 AM)
     * @param bSaveFields boolean
     * @return boolean - true if successful
     */
    private boolean updateScenarioFields(boolean bSaveFields)
    {

        //String methodName = "*** updateScenarioFields";
        //if (_DEBUG) System.out.println(methodName);

        int selectedIndex = getListScenario().getSelectedIndex();

        // *** return immediately if nothing selected or last one selected && no editing done 
        if (selectedIndex == -1
                || (selectedIndex == getlistModelScenario().getSize() - 1 && nScenarioEditing == -1))
            return true;

        if (bSaveFields == true && nScenarioEditing != -1)
        {
            // if (bSaveFields == true && nScenarioEditing != -1 && !(nScenarioEditing > currentData.scenarios.size())) {
            // *** transfer data from GUI to correct ModelScenario structure

            ModelScenario editedScenario = (ModelScenario) currentData.scenarios
                    .get(nScenarioEditing);

            // check to see if keys (data listed in Scenario List) have changed, if so, update the list
            boolean bKeysEdited = false; // initialize
            if (!editedScenario.source.equals(gettextSource().getText())
                    || !editedScenario.name.equals(EnumeratedTypes.getCode(
                            EnumeratedTypes.FIELD_SCENARIO,
                            (String) getcomboScenario().getSelectedItem())))
            {
                bKeysEdited = true;

                // are the new keys unique?
                boolean bUniqueKeys = true;
                for (int i = 0; i < currentData.scenarios.size(); i++)
                {
                    if (i == nScenarioEditing)
                        continue;
                    ModelScenario s = (ModelScenario) currentData.scenarios
                            .get(i);
                    if (gettextSource().getText().equals(s.source)
                            && s.name.equals(EnumeratedTypes.getCode(
                                    EnumeratedTypes.FIELD_SCENARIO,
                                    (String) getcomboScenario()
                                            .getSelectedItem())))
                        bUniqueKeys = false;
                }
                if (!bUniqueKeys)
                    return false;
            }
            String testString = gettextSource().getText().trim();
            if (!editedScenario.source.equalsIgnoreCase(testString))
            {
                editedScenario.source = testString;
                editedScenario.changeFlag = 1;
                bChangedData = true;
            }
            testString = EnumeratedTypes.getCode(
                    EnumeratedTypes.FIELD_SCENARIO, (String) getcomboScenario()
                            .getSelectedItem());
            if (!editedScenario.name.equalsIgnoreCase(testString))
            {
                editedScenario.name = testString;
                editedScenario.changeFlag = 1;
                bChangedData = true;
            }
            if (editedScenario.damType != getcomboTypeOfDam()
                    .getSelectedIndex())
            {
                editedScenario.damType = getcomboTypeOfDam().getSelectedIndex();
                editedScenario.changeFlag = 1;
                bChangedData = true;
            }
            if (editedScenario.HDE != NumFormat.toFloat(gettextHDE().getText()))
            {
                editedScenario.HDE = NumFormat.toFloat(gettextHDE().getText());
                editedScenario.changeFlag = 1;
                bChangedData = true;
            }
            if (editedScenario.BME != NumFormat.toFloat(gettextBME().getText()))
            {
                editedScenario.BME = NumFormat.toFloat(gettextBME().getText());
                editedScenario.changeFlag = 1;
                bChangedData = true;
            }
            if (editedScenario.VOL != NumFormat.toFloat(gettextVOL().getText()))
            {
                editedScenario.VOL = NumFormat.toFloat(gettextVOL().getText());
                editedScenario.changeFlag = 1;
                bChangedData = true;
            }
            if (editedScenario.SA != NumFormat.toFloat(gettextSA().getText()))
            {
                editedScenario.SA = NumFormat.toFloat(gettextSA().getText());
                editedScenario.changeFlag = 1;
                bChangedData = true;
            }
            if (editedScenario.BW != NumFormat.toFloat(gettextBW().getText()))
            {
                editedScenario.BW = NumFormat.toFloat(gettextBW().getText());
                editedScenario.changeFlag = 1;
                bChangedData = true;
            }
            if (editedScenario.TFM != NumFormat.toFloat(gettextTFM().getText()))
            {
                editedScenario.TFM = NumFormat.toFloat(gettextTFM().getText());
                editedScenario.changeFlag = 1;
                bChangedData = true;
            }
            if (editedScenario.QO != NumFormat.toFloat(gettextQO().getText()))
            {
                editedScenario.QO = NumFormat.toFloat(gettextQO().getText());
                editedScenario.changeFlag = 1;
                bChangedData = true;
            }
            if (editedScenario.DISTTN != NumFormat.toFloat(gettextDISTTN()
                    .getText()))
            {
                editedScenario.DISTTN = NumFormat.toFloat(gettextDISTTN()
                        .getText());
                editedScenario.changeFlag = 1;
                bChangedData = true;
            }
            if (editedScenario.CMS != NumFormat.toFloat(gettextCMS().getText()))
            {
                editedScenario.CMS = NumFormat.toFloat(gettextCMS().getText());
                editedScenario.changeFlag = 1;
                bChangedData = true;
            }
            if (editedScenario.dbugType != getcomboDBUG().getSelectedIndex())
            {
                editedScenario.dbugType = getcomboDBUG().getSelectedIndex();
                editedScenario.changeFlag = 1;
                bChangedData = true;
            }
            // update the scenario list if the keys have been edited
            if (bKeysEdited)
                updateScenarioList();

        } else if (bSaveFields == false)
        {
            // *** Move data from Logical Data Structure to GUI 

            ModelScenario selectedScenario = (ModelScenario) currentData.scenarios
                    .get(selectedIndex);

            nScenarioEditing = selectedIndex;

            gettextSource().setText(selectedScenario.source);
            getcomboScenario().setSelectedItem(
                    EnumeratedTypes.getEnglish(EnumeratedTypes.FIELD_SCENARIO,
                            selectedScenario.name));
            getcomboTypeOfDam().setSelectedIndex(selectedScenario.damType);
            gettextHDE().setText(NumFormat.format(selectedScenario.HDE, 4));
            gettextBME().setText(NumFormat.format(selectedScenario.BME, 4));
            gettextVOL().setText(NumFormat.format(selectedScenario.VOL, 4));
            gettextSA().setText(NumFormat.format(selectedScenario.SA, 4));
            gettextBW().setText(NumFormat.format(selectedScenario.BW, 4));
            gettextTFM().setText(NumFormat.format(selectedScenario.TFM, 4));
            gettextQO().setText(NumFormat.format(selectedScenario.QO, 4));
            gettextDISTTN().setText(
                    NumFormat.format(selectedScenario.DISTTN, 4));
            gettextCMS().setText(NumFormat.format(selectedScenario.CMS, 4));
            getcomboDBUG().setSelectedIndex(selectedScenario.dbugType);
        }
        return true;
    }

    /**
     * Synchronizes the scenario data structures with the list.
     * Preserves the selected scenario;  
     *     (if nothing selected defaults to first one)
     *
     * Creation date: (7/29/2003 9:32:06 AM)
     */
    private void updateScenarioList()
    {

        int count = 0;

        int oldSelection = getListScenario().getSelectedIndex();

        getlistModelScenario().clear();

        for (int i = 0; i < currentData.scenarios.size(); i++)
        {
            ModelScenario scenario = (ModelScenario) currentData.scenarios
                    .get(i);
            getlistModelScenario().addElement(
                    scenario.source
                            + " - "
                            + EnumeratedTypes.getEnglish(
                                    EnumeratedTypes.FIELD_SCENARIO,
                                    scenario.name));
            // *** set pointOfInterestDistance to max found in all scenarios
            if (scenario.DISTTN > currentData.pointOfInterestDistance)
                currentData.pointOfInterestDistance = scenario.DISTTN;

            String changedSource = scenario.source; // skip scenarios started with '#' when scenario is highlighted initially 
            if (changedSource.startsWith("#"))
            {
                count++;
            }
        }

        getlistModelScenario().addElement(
                "Highlight me to append a new scenario");

        if (oldSelection == -1 && getlistModelScenario().size() > 1) // test if more than the "Highlight me " entry
            oldSelection = count; // if so, set to first one

        getListScenario().setSelectedIndex(oldSelection);
        _selectedScenario = oldSelection;

    }

    /**
     * Update Cross Section
     * Synchronizes the editors and the data backing them up.
     *
     * If the parameter is true, the data in the editors is saved to the data structure; 
     *
     * if the parameter is false, data in the editors is replaced by data in the structure.
     * 
     * Creation date: (7/30/2003 7:51:19 AM)
     * @param bSaveFields boolean
     */
    private boolean updateXSFields(boolean bSaveFields)
    {

        String methodName = "*** updateXSFields";
        //if (_DEBUG) System.out.println(methodName);

        // *** this assumes JLists in GUI matches ArrayLists maintained within AnalysisData structure 
        int selectedDS = getListDown().getSelectedIndex();
        int selectedXS = getListXS().getSelectedIndex();

        if (selectedXS == -1
                || (selectedXS == getlistModelXS().getSize() - 1 && nXSEditing == -1))
        {
            return true;
        }

        DownstreamPoint editedDown = (DownstreamPoint) currentData.downstream
                .get(nDownstreamEditing);

        if (bSaveFields == true && nXSEditing != -1) // *** Move data from GUI to Logical Data Structure 
        {
            SectionGeometry editedSection = (SectionGeometry) editedDown.xsections
                    .get(nXSEditing);

            // check to see if keys (data listed in Cross Section Type List) have changed, if so, update the list later
            boolean bKeysEdited = false;

            if (!editedSection.getXSType().equals(
                    EnumeratedTypes.getCode(EnumeratedTypes.FIELD_XSTYPE,
                            (String) getcomboXSType().getSelectedItem())))
            {
                bKeysEdited = true;

                // are the new keys unique?
                boolean bUniqueKeys = true;
                for (int i = 0; i < editedDown.xsections.size(); i++)
                {
                    if (i == nXSEditing)
                        continue;
                    SectionGeometry sg = (SectionGeometry) editedDown.xsections
                            .get(i);
                    if (sg.getXSType().equals(
                            EnumeratedTypes
                                    .getCode(EnumeratedTypes.FIELD_XSTYPE,
                                            (String) getcomboXSType()
                                                    .getSelectedItem())))
                        bUniqueKeys = false;
                }
                if (!bUniqueKeys)
                    return false;
            }
            String testString = EnumeratedTypes.getCode(
                    EnumeratedTypes.FIELD_XSTYPE, (String) getcomboXSType()
                            .getSelectedItem());
            if (!editedSection.getXSType().equalsIgnoreCase(testString))
            {
                editedSection.setXSType(testString);
                editedSection.setChangeFlag(1);
                bChangedData = true;
            }

            // Save geometry table
            stopTableEditing();
            for (int i = 0; i < 8; i++)
            {
                if (((String) gettableModelXSGeom().getValueAt(i, 0))
                        .equals(""))
                {
                    editedSection.setElevationData(i, 0, -999.0f);
                    editedSection.setElevationData(i, 1, 0.0f);
                    editedSection.setElevationData(i, 2, 0.0f);
                    editedSection.setElevationData(i, 3, 0.0f);
                    continue;
                }
                editedSection.setLastRowUsed(i);
                float elevData = NumFormat
                        .toFloat((String) gettableModelXSGeom()
                                .getValueAt(i, 0));
                if (editedSection.getElevationData(i, 0) != elevData)
                {
                    editedSection.setElevationData(i, 0, NumFormat
                            .toFloat((String) gettableModelXSGeom().getValueAt(
                                    i, 0)));
                    editedSection.setChangeFlag(1);
                    bChangedData = true;
                }
                elevData = NumFormat.toFloat((String) gettableModelXSGeom()
                        .getValueAt(i, 1));
                if (editedSection.getElevationData(i, 1) != elevData)
                {
                    editedSection.setElevationData(i, 1, NumFormat
                            .toFloat((String) gettableModelXSGeom().getValueAt(
                                    i, 1)));
                    editedSection.setChangeFlag(1);
                    bChangedData = true;
                }
                elevData = NumFormat.toFloat((String) gettableModelXSGeom()
                        .getValueAt(i, 2));
                if (editedSection.getElevationData(i, 2) != elevData)
                {
                    editedSection.setElevationData(i, 2, NumFormat
                            .toFloat((String) gettableModelXSGeom().getValueAt(
                                    i, 2)));
                    editedSection.setChangeFlag(1);
                    bChangedData = true;
                }
                elevData = NumFormat.toFloat((String) gettableModelXSGeom()
                        .getValueAt(i, 3));
                if (editedSection.getElevationData(i, 3) != elevData)
                {
                    editedSection.setElevationData(i, 3, NumFormat
                            .toFloat((String) gettableModelXSGeom().getValueAt(
                                    i, 3)));
                    editedSection.setChangeFlag(1);
                    bChangedData = true;
                }
            }

            // if the keys have been edited update the cross section type list 
            if (bKeysEdited)
                updateXSList();

        } else if (bSaveFields == false)
        {
            // *** Move data from Logical Data Structure To GUI

            SectionGeometry selectedSection = (SectionGeometry) editedDown.xsections
                    .get(selectedXS);

            nXSEditing = selectedXS;

            getcomboXSType().setSelectedItem(
                    EnumeratedTypes.getEnglish(EnumeratedTypes.FIELD_XSTYPE,
                            selectedSection.getXSType()));

            // Load geometry table
            stopTableEditing();
            for (int i = 0; i < 8; i++)
            {
                if (selectedSection.getElevationData(i, 0) == -999.0f)
                {
                    gettableModelXSGeom().setValueAt("", i, 0);
                    gettableModelXSGeom().setValueAt("", i, 1);
                    gettableModelXSGeom().setValueAt("", i, 2);
                    gettableModelXSGeom().setValueAt("", i, 3); // was set to 0.05
                } else
                {
                    gettableModelXSGeom().setValueAt(
                            NumFormat.format(selectedSection.getElevationData(
                                    i, 0), 4), i, 0);
                    gettableModelXSGeom().setValueAt(
                            NumFormat.format(selectedSection.getElevationData(
                                    i, 1), 4), i, 1);
                    gettableModelXSGeom().setValueAt(
                            NumFormat.format(selectedSection.getElevationData(
                                    i, 2), 4), i, 2);
                    gettableModelXSGeom().setValueAt(
                            NumFormat.format(selectedSection.getElevationData(
                                    i, 3), 4), i, 3);
                }
            }
        }

        return true;

    }

    /**
     * Synchronizes the xs data structure for the chosen
     * downstream point with the list. Preserves the
     * selected cross section.
     *
     * Creation date: (7/29/2003 9:32:06 AM)
     */
    private void updateXSList()
    {

        // *** check to see if DownstreamPoint is selected
        // _selectedDownstreamPoint = getListDown().getSelectedIndex(); 
        int selectedDS = getListDown().getSelectedIndex();

        // if (nDownstreamEditing == -1)
        if (selectedDS == -1)
        {
            return;
        }

        int oldSelection = getListXS().getSelectedIndex();

        getlistModelXS().clear();

        // DownstreamPoint down = (DownstreamPoint)currentData.downstream.get(nDownstreamEditing);
        DownstreamPoint down = (DownstreamPoint) currentData.downstream
                .get(selectedDS);
        int bestXS = -1;

        // *** this recomputes best XS index !
        // *** is this necessary ?	
        for (int i = 0; i < down.xsections.size(); i++)
        {
            SectionGeometry xs = (SectionGeometry) down.xsections.get(i);
            getlistModelXS().addElement(
                    "Type: "
                            + EnumeratedTypes.getEnglish(
                                    EnumeratedTypes.FIELD_XSTYPE, xs
                                            .getXSType()));
            // if (_DEBUG) System.out.println("XSType = " + xs.getXSType());
            // if (_DEBUG) System.out.println("down.xsecBestType = " + down.xsecBestType);
            if (xs.getXSType().equalsIgnoreCase(down.xsecBestType))
            {
                bestXS = i;
            }
        }
        getlistModelXS().addElement(
                "Highlight me to append a new set of elevations");

        if (oldSelection == -1 && bestXS > -1)
        {
            oldSelection = bestXS;
        }

        _selectedXSType = oldSelection;

        getListXS().setSelectedIndex(_selectedXSType);

    }
}