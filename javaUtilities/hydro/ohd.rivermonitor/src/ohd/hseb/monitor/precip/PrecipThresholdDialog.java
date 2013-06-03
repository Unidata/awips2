package ohd.hseb.monitor.precip;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.TitledBorder;

import ohd.hseb.db.DbTable;
import ohd.hseb.monitor.precip.settings.PrecipColumnDataSettings;
import ohd.hseb.util.MathHelper;
import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.WindowResizingManager;

public class PrecipThresholdDialog extends JDialog
{
    private JSpinner _precipAlert1HrSpinBox ;
    private JSpinner _precipAlert3HrSpinBox ;
    private JSpinner _precipAlert6HrSpinBox ;

    private JSpinner _precipCaution1HrSpinBox ;
    private JSpinner _precipCaution3HrSpinBox ;
    private JSpinner _precipCaution6HrSpinBox ;

    private JSpinner _ratioAlert1HrSpinBox ;
    private JSpinner _ratioAlert3HrSpinBox ;
    private JSpinner _ratioAlert6HrSpinBox ;

    private JSpinner _ratioCaution1HrSpinBox ;
    private JSpinner _ratioCaution3HrSpinBox ;
    private JSpinner _ratioCaution6HrSpinBox ;

    private JSpinner _diffAlert1HrSpinBox ;
    private JSpinner _diffAlert3HrSpinBox ;
    private JSpinner _diffAlert6HrSpinBox ;

    private JSpinner _diffCaution1HrSpinBox ;
    private JSpinner _diffCaution3HrSpinBox ;
    private JSpinner _diffCaution6HrSpinBox ;

    private JButton _applyButton;
    private JButton _closeButton;

    private SessionLogger _logger;

    private PrecipColumnDataSettings _prevMenuSettings = null;
    private PrecipColumnDataSettings _savedMenuSettings = null;
    
    public PrecipThresholdDialog( JFrame mainFrame, PrecipColumnDataSettings menuSettings, SessionLogger logger)
    {
        super(mainFrame, true);
        _logger = logger;
        _prevMenuSettings = menuSettings;
        createPrecipSettingsDialog();
    }
    
    public void setMenuSettingsForDialog(PrecipColumnDataSettings menuSettings)
    {
        _prevMenuSettings = menuSettings;
        createPrecipSettingsDialog();
    }
    
    public PrecipColumnDataSettings showPrecipThresholdDialog()
    {
        _savedMenuSettings = null;
        this.setVisible(true);
        return _savedMenuSettings;
    }
    
    public void createPrecipSettingsDialog()
    {
        JPanel precipPanel = new JPanel(new GridBagLayout());
        TitledBorder precipTitledBorder = BorderFactory.createTitledBorder("Precip Threshold");
        precipPanel.setBorder(precipTitledBorder);
        precipPanel.setToolTipText("Applied to 1hr, 3hr, 6hr Latest Precip and TOH Precip");

        if(_prevMenuSettings == null)
        {
            _prevMenuSettings = new PrecipColumnDataSettings();
        }

        JLabel precip1HrLabel = new JLabel("1 Hr");
        precip1HrLabel.setPreferredSize(new Dimension(40, 30));
        JLabel precip3HrLabel = new JLabel("3 Hr");
        precip3HrLabel.setPreferredSize(new Dimension(40, 30));
        JLabel precip6HrLabel = new JLabel("6 Hr");
        precip6HrLabel.setPreferredSize(new Dimension(40, 30));

        JPanel precipLabelPanel = new JPanel();
        precipLabelPanel.setPreferredSize(new Dimension(150, 40));
        precipLabelPanel.add(precip1HrLabel);
        precipLabelPanel.add(precip3HrLabel);
        precipLabelPanel.add(precip6HrLabel);

        //initial value, min value,                                  max value,                                   incr value   
        _precipAlert1HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getPrecipAlert1Hr(), PrecipColumnDataSettings.DEFAULT_PRECIP_MIN_THRESHOLD_1HR,PrecipColumnDataSettings.DEFAULT_PRECIP_MAX_THRESHOLD_1HR, 0.01));
        _precipAlert1HrSpinBox.setPreferredSize(new Dimension(45, 30));
        _precipAlert3HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getPrecipAlert3Hr(), PrecipColumnDataSettings.DEFAULT_PRECIP_MIN_THRESHOLD_3HR,PrecipColumnDataSettings.DEFAULT_PRECIP_MAX_THRESHOLD_3HR, 0.01));
        _precipAlert3HrSpinBox.setPreferredSize(new Dimension(45, 30));
        _precipAlert6HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getPrecipAlert6Hr(), PrecipColumnDataSettings.DEFAULT_PRECIP_MIN_THRESHOLD_6HR,PrecipColumnDataSettings.DEFAULT_PRECIP_MAX_THRESHOLD_6HR, 0.01));
        _precipAlert6HrSpinBox.setPreferredSize(new Dimension(45, 30));

        JPanel precipAlertPanel = new JPanel();
        precipAlertPanel.setPreferredSize(new Dimension(150, 40));
        precipAlertPanel.add(_precipAlert1HrSpinBox);
        precipAlertPanel.add(_precipAlert3HrSpinBox);
        precipAlertPanel.add(_precipAlert6HrSpinBox);

        _precipCaution1HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getPrecipCaution1Hr(), PrecipColumnDataSettings.DEFAULT_PRECIP_MIN_THRESHOLD_1HR,PrecipColumnDataSettings.DEFAULT_PRECIP_MAX_THRESHOLD_1HR, 0.01));
        _precipCaution1HrSpinBox.setPreferredSize(new Dimension(45, 30));
        _precipCaution3HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getPrecipCaution3Hr(), PrecipColumnDataSettings.DEFAULT_PRECIP_MIN_THRESHOLD_3HR,PrecipColumnDataSettings.DEFAULT_PRECIP_MAX_THRESHOLD_3HR, 0.01));
        _precipCaution3HrSpinBox.setPreferredSize(new Dimension(45, 30));
        _precipCaution6HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getPrecipCaution6Hr(), PrecipColumnDataSettings.DEFAULT_PRECIP_MIN_THRESHOLD_6HR,PrecipColumnDataSettings.DEFAULT_PRECIP_MAX_THRESHOLD_6HR, 0.01));
        _precipCaution6HrSpinBox.setPreferredSize(new Dimension(45, 30));

        JPanel precipCautionPanel = new JPanel();
        precipCautionPanel.setPreferredSize(new Dimension(150, 40));
        precipCautionPanel.add(_precipCaution1HrSpinBox);
        precipCautionPanel.add(_precipCaution3HrSpinBox);
        precipCautionPanel.add(_precipCaution6HrSpinBox);

        JPanel precipDummyPanel = new JPanel();
        precipDummyPanel.setPreferredSize(new Dimension(150, 10));
        ComponentHelper.addPanelComponent(precipPanel, precipLabelPanel, 0, 0, 1, 1, 1);
        ComponentHelper.addPanelComponent(precipPanel, precipAlertPanel, 0, 1, 1, 1, 1);
        ComponentHelper.addPanelComponent(precipPanel, precipDummyPanel, 0, 2, 1, 1, 1);
        ComponentHelper.addPanelComponent(precipPanel, precipCautionPanel, 0, 3, 1, 1, 1);

        JPanel ratioPanel = new JPanel(new GridBagLayout());
        TitledBorder ratioTitledBorder = BorderFactory.createTitledBorder("Ratio(%) Threshold");
        ratioPanel.setBorder(ratioTitledBorder);
        ratioPanel.setToolTipText("Applied to ((Latest Precip / FFG) * 100) columns");

        JLabel ratio1HrLabel = new JLabel("1 Hr");
        ratio1HrLabel.setPreferredSize(new Dimension(40, 30));
        JLabel ratio3HrLabel = new JLabel("3 Hr");
        ratio3HrLabel.setPreferredSize(new Dimension(40, 30));
        JLabel ratio6HrLabel = new JLabel("6 Hr");
        ratio6HrLabel.setPreferredSize(new Dimension(40, 30));

        JPanel ratioLabelPanel = new JPanel();
        ratioLabelPanel.setPreferredSize(new Dimension(150, 40));
        ratioLabelPanel.add(ratio1HrLabel);
        ratioLabelPanel.add(ratio3HrLabel);
        ratioLabelPanel.add(ratio6HrLabel);

        //      initial value, min value,                                  max value,                                   incr value
        _ratioAlert1HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getRatioAlert1Hr(), PrecipColumnDataSettings.DEFAULT_RATIO_MIN_THRESHOLD_1HR,PrecipColumnDataSettings.DEFAULT_RATIO_MAX_THRESHOLD_1HR, 1));
        _ratioAlert1HrSpinBox.setPreferredSize(new Dimension(45, 30));
        _ratioAlert3HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getRatioAlert3Hr(), PrecipColumnDataSettings.DEFAULT_RATIO_MIN_THRESHOLD_1HR,PrecipColumnDataSettings.DEFAULT_RATIO_MAX_THRESHOLD_1HR, 1));
        _ratioAlert3HrSpinBox.setPreferredSize(new Dimension(45, 30));
        _ratioAlert6HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getRatioAlert6Hr(), PrecipColumnDataSettings.DEFAULT_RATIO_MIN_THRESHOLD_1HR,PrecipColumnDataSettings.DEFAULT_RATIO_MAX_THRESHOLD_1HR, 1));
        _ratioAlert6HrSpinBox.setPreferredSize(new Dimension(45, 30));

        JPanel ratioAlertPanel = new JPanel();
        ratioAlertPanel.setPreferredSize(new Dimension(150, 40));
        ratioAlertPanel.add(_ratioAlert1HrSpinBox);
        ratioAlertPanel.add(_ratioAlert3HrSpinBox);
        ratioAlertPanel.add(_ratioAlert6HrSpinBox);

        //      initial value, min value,                                  max value,                                   incr value
        _ratioCaution1HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getRatioCaution1Hr(), PrecipColumnDataSettings.DEFAULT_RATIO_MIN_THRESHOLD_1HR,PrecipColumnDataSettings.DEFAULT_RATIO_MAX_THRESHOLD_1HR, 1));
        _ratioCaution1HrSpinBox.setPreferredSize(new Dimension(45, 30));
        _ratioCaution3HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getRatioCaution3Hr(), PrecipColumnDataSettings.DEFAULT_RATIO_MIN_THRESHOLD_1HR,PrecipColumnDataSettings.DEFAULT_RATIO_MAX_THRESHOLD_1HR, 1));
        _ratioCaution3HrSpinBox.setPreferredSize(new Dimension(45, 30));
        _ratioCaution6HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getRatioCaution6Hr(), PrecipColumnDataSettings.DEFAULT_RATIO_MIN_THRESHOLD_1HR,PrecipColumnDataSettings.DEFAULT_RATIO_MAX_THRESHOLD_1HR, 1));
        _ratioCaution6HrSpinBox.setPreferredSize(new Dimension(45, 30));

        JPanel ratioCautionPanel = new JPanel();
        ratioCautionPanel.setPreferredSize(new Dimension(150, 40));
        ratioCautionPanel.add(_ratioCaution1HrSpinBox);
        ratioCautionPanel.add(_ratioCaution3HrSpinBox);
        ratioCautionPanel.add(_ratioCaution6HrSpinBox);

        JPanel ratioDummyPanel = new JPanel();
        ratioDummyPanel.setPreferredSize(new Dimension(150, 10));
        ComponentHelper.addPanelComponent(ratioPanel, ratioLabelPanel, 0, 0, 1, 1, 1);
        ComponentHelper.addPanelComponent(ratioPanel, ratioAlertPanel, 0, 1, 1, 1, 1);
        ComponentHelper.addPanelComponent(ratioPanel, ratioDummyPanel, 0, 2, 1, 1, 1);
        ComponentHelper.addPanelComponent(ratioPanel, ratioCautionPanel, 0, 3, 1, 1, 1);

        JPanel diffPanel = new JPanel(new GridBagLayout());
        TitledBorder diffTitledBorder = BorderFactory.createTitledBorder("Diff Threshold");
        diffPanel.setBorder(diffTitledBorder);
        diffPanel.setToolTipText("Applied to (Latest Precip - FFG) columns");

        JLabel diff1HrLabel = new JLabel("1 Hr");
        diff1HrLabel.setPreferredSize(new Dimension(40, 30));
        JLabel diff3HrLabel = new JLabel("3 Hr");
        diff3HrLabel.setPreferredSize(new Dimension(40, 30));
        JLabel diff6HrLabel = new JLabel("6 Hr");
        diff6HrLabel.setPreferredSize(new Dimension(40, 30));

        JPanel diffLabelPanel = new JPanel();
        diffLabelPanel.setPreferredSize(new Dimension(150, 40));
        diffLabelPanel.add(diff1HrLabel);
        diffLabelPanel.add(diff3HrLabel);
        diffLabelPanel.add(diff6HrLabel);

        //         initial value, min value,                                  max value,                                   incr value                                                                
        _diffAlert1HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getDiffAlert1Hr(), PrecipColumnDataSettings.DEFAULT_DIFF_MIN_THRESHOLD_1HR,PrecipColumnDataSettings.DEFAULT_DIFF_MAX_THRESHOLD_1HR, 0.05));
        _diffAlert1HrSpinBox.setPreferredSize(new Dimension(55, 30));
        _diffAlert3HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getDiffAlert3Hr(), PrecipColumnDataSettings.DEFAULT_DIFF_MIN_THRESHOLD_1HR,PrecipColumnDataSettings.DEFAULT_DIFF_MAX_THRESHOLD_1HR, 0.05));
        _diffAlert3HrSpinBox.setPreferredSize(new Dimension(55, 30));
        _diffAlert6HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getDiffAlert6Hr(), PrecipColumnDataSettings.DEFAULT_DIFF_MIN_THRESHOLD_1HR,PrecipColumnDataSettings.DEFAULT_DIFF_MAX_THRESHOLD_1HR, 0.05));
        _diffAlert6HrSpinBox.setPreferredSize(new Dimension(55, 30));

        JPanel diffAlertPanel = new JPanel();
        diffAlertPanel.setPreferredSize(new Dimension(180, 40));
        diffAlertPanel.add(_diffAlert1HrSpinBox);
        diffAlertPanel.add(_diffAlert3HrSpinBox);
        diffAlertPanel.add(_diffAlert6HrSpinBox);

        _diffCaution1HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getDiffCaution1Hr(), PrecipColumnDataSettings.DEFAULT_DIFF_MIN_THRESHOLD_1HR,PrecipColumnDataSettings.DEFAULT_DIFF_MAX_THRESHOLD_1HR, 0.05));
        _diffCaution1HrSpinBox.setPreferredSize(new Dimension(55, 30));
        _diffCaution3HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getDiffCaution3Hr(), PrecipColumnDataSettings.DEFAULT_DIFF_MIN_THRESHOLD_1HR,PrecipColumnDataSettings.DEFAULT_DIFF_MAX_THRESHOLD_1HR, 0.05));
        _diffCaution3HrSpinBox.setPreferredSize(new Dimension(55, 30));
        _diffCaution6HrSpinBox = new JSpinner(new SpinnerNumberModel(_prevMenuSettings.getDiffCaution6Hr(), PrecipColumnDataSettings.DEFAULT_DIFF_MIN_THRESHOLD_1HR,PrecipColumnDataSettings.DEFAULT_DIFF_MAX_THRESHOLD_1HR, 0.05));
        _diffCaution6HrSpinBox.setPreferredSize(new Dimension(55, 30));

        JPanel diffCautionPanel = new JPanel();
        diffCautionPanel.setPreferredSize(new Dimension(180, 40));
        diffCautionPanel.add(_diffCaution1HrSpinBox);
        diffCautionPanel.add(_diffCaution3HrSpinBox);
        diffCautionPanel.add(_diffCaution6HrSpinBox);

        JPanel diffDummyPanel = new JPanel();
        diffDummyPanel.setPreferredSize(new Dimension(180, 10));
        ComponentHelper.addPanelComponent(diffPanel, diffLabelPanel, 0, 0, 1, 1, 1);
        ComponentHelper.addPanelComponent(diffPanel, diffAlertPanel, 0, 1, 1, 1, 1);
        ComponentHelper.addPanelComponent(diffPanel, diffDummyPanel, 0, 2, 1, 1, 1);
        ComponentHelper.addPanelComponent(diffPanel, diffCautionPanel, 0, 3, 1, 1, 1);

        JLabel alertLabel = new JLabel("Alert      :");
        alertLabel.setOpaque(true);
        alertLabel.setBackground(Color.RED);
        JLabel cautionLabel = new JLabel("Caution :");
        cautionLabel.setOpaque(true);
        cautionLabel.setBackground(Color.YELLOW);

        JPanel alertCautionPanel = new JPanel(new GridBagLayout());

        JPanel dummyPanel1 = new JPanel();
        dummyPanel1.setPreferredSize(new Dimension(60, 40));

        alertLabel.setPreferredSize(new Dimension(60, 40));

        JPanel dummyPanel2 = new JPanel();
        dummyPanel2.setPreferredSize(new Dimension(60, 10));

        cautionLabel.setPreferredSize(new Dimension(60, 40));

        ComponentHelper.addPanelComponent(alertCautionPanel, dummyPanel1, 0, 0, 1, 1, 1);
        ComponentHelper.addPanelComponent(alertCautionPanel, alertLabel, 0, 1, 1, 1, 1);
        ComponentHelper.addPanelComponent(alertCautionPanel, dummyPanel2, 0, 2, 1, 1, 1);
        ComponentHelper.addPanelComponent(alertCautionPanel, cautionLabel, 0, 3, 1, 1, 1);

        JPanel thresholdPanel = new JPanel(new GridBagLayout());
        ComponentHelper.addPanelComponent(thresholdPanel, alertCautionPanel, 0, 0, 1, 1, 1);
        ComponentHelper.addPanelComponent(thresholdPanel, precipPanel, 1, 0, 1, 1, 1);
        ComponentHelper.addPanelComponent(thresholdPanel, ratioPanel, 2, 0, 1, 1, 1);
        ComponentHelper.addPanelComponent(thresholdPanel, diffPanel, 3, 0, 1, 1, 1);

        JPanel buttonPanel = new JPanel(new GridBagLayout());

        JPanel dummyPanel3 = new JPanel();
        dummyPanel3.setPreferredSize(new Dimension(20,20));
 
        _closeButton = new JButton("Close");
        _applyButton = new JButton ("Apply");
        _applyButton.addActionListener(new ApplyMenuSettingsListener());
        _closeButton.addActionListener(new ClosePrecipSettingsDialogListener());

        ComponentHelper.addPanelComponent(buttonPanel, _applyButton, 0, 0, 1, 1, 1);
        ComponentHelper.addPanelComponent(buttonPanel, dummyPanel3, 1, 0, 1, 1, 1);
        ComponentHelper.addPanelComponent(buttonPanel, _closeButton, 2, 0, 1, 1, 1);

        JPanel dummyPanel5 = new JPanel();
        dummyPanel5.setPreferredSize(new Dimension(200,40));

        JPanel outerPanel = new JPanel(new GridBagLayout());
        ComponentHelper.addPanelComponent(outerPanel, thresholdPanel, 0, 0, 1, 1, 1);
        ComponentHelper.addPanelComponent(outerPanel, dummyPanel5, 0, 1, 1, 1, 1);
        ComponentHelper.addPanelComponent(outerPanel, buttonPanel, 0, 2, 1, 1, 1);

        this.getContentPane().add(outerPanel);
        this.setLocation(25, 25);
        Dimension dim = new Dimension(600,450);
        new WindowResizingManager(this, dim, dim);
       
        this.pack();
        this.setTitle("Precip Alert/Caution Threshold Values");
    }

    private void closePrecipSettingsDialog()
    {
        this.setVisible(false);
        this.dispose();
    }

//  ---------------------------------------------------------------------------------------------------

    private double roundTo2DecimalPlaces(double number)
    {
        double result = DbTable.getNullDouble();
        if (number != DbTable.getNullDouble())
            result = MathHelper.roundToNDecimalPlaces(number, 2);

        return result;
    }
    
    private PrecipColumnDataSettings getPrecipSettingsFromDialog()
    {
        PrecipColumnDataSettings menuSettings = new PrecipColumnDataSettings();
        
        menuSettings.setPrecipAlert1Hr(roundTo2DecimalPlaces((Double)_precipAlert1HrSpinBox.getValue()));
        menuSettings.setPrecipAlert3Hr(roundTo2DecimalPlaces((Double)_precipAlert3HrSpinBox.getValue()));
        menuSettings.setPrecipAlert6Hr(roundTo2DecimalPlaces((Double)_precipAlert6HrSpinBox.getValue()));
        menuSettings.setPrecipCaution1Hr(roundTo2DecimalPlaces((Double)_precipCaution1HrSpinBox.getValue()));
        menuSettings.setPrecipCaution3Hr(roundTo2DecimalPlaces((Double)_precipCaution3HrSpinBox.getValue()));
        menuSettings.setPrecipCaution6Hr(roundTo2DecimalPlaces((Double)_precipCaution6HrSpinBox.getValue()));

        menuSettings.setRatioAlert1Hr((Integer)_ratioAlert1HrSpinBox.getValue());
        menuSettings.setRatioAlert3Hr((Integer)_ratioAlert3HrSpinBox.getValue());
        menuSettings.setRatioAlert6Hr((Integer)_ratioAlert6HrSpinBox.getValue());
        menuSettings.setRatioCaution1Hr((Integer)_ratioCaution1HrSpinBox.getValue());
        menuSettings.setRatioCaution3Hr((Integer)_ratioCaution3HrSpinBox.getValue());
        menuSettings.setRatioCaution6Hr((Integer)_ratioCaution6HrSpinBox.getValue());

        menuSettings.setDiffAlert1Hr(roundTo2DecimalPlaces((Double)_diffAlert1HrSpinBox.getValue()));
        menuSettings.setDiffAlert3Hr(roundTo2DecimalPlaces((Double)_diffAlert3HrSpinBox.getValue()));
        menuSettings.setDiffAlert6Hr(roundTo2DecimalPlaces((Double)_diffAlert6HrSpinBox.getValue()));
        menuSettings.setDiffCaution1Hr(roundTo2DecimalPlaces((Double)_diffCaution1HrSpinBox.getValue()));
        menuSettings.setDiffCaution3Hr(roundTo2DecimalPlaces((Double)_diffCaution3HrSpinBox.getValue()));
        menuSettings.setDiffCaution6Hr(roundTo2DecimalPlaces((Double)_diffCaution6HrSpinBox.getValue()));

        return menuSettings;
    }
    
    private class ApplyMenuSettingsListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            PrecipColumnDataSettings currentMenuSettings = getPrecipSettingsFromDialog();
            
            if( (_prevMenuSettings.getPrecipAlert1Hr() != currentMenuSettings.getPrecipAlert1Hr()) ||
                    (_prevMenuSettings.getPrecipAlert3Hr() != currentMenuSettings.getPrecipAlert3Hr()) ||
                    (_prevMenuSettings.getPrecipAlert6Hr() != currentMenuSettings.getPrecipAlert6Hr()) ||
                    (_prevMenuSettings.getRatioAlert1Hr() != currentMenuSettings.getRatioAlert1Hr()) ||
                    (_prevMenuSettings.getRatioAlert3Hr() != currentMenuSettings.getRatioAlert3Hr()) ||
                    (_prevMenuSettings.getRatioAlert6Hr() != currentMenuSettings.getRatioAlert6Hr()) ||
                    (_prevMenuSettings.getDiffAlert1Hr() != currentMenuSettings.getDiffAlert1Hr()) ||
                    (_prevMenuSettings.getDiffAlert3Hr() != currentMenuSettings.getDiffAlert3Hr()) ||
                    (_prevMenuSettings.getDiffAlert6Hr() != currentMenuSettings.getDiffAlert6Hr()) ||
                    (_prevMenuSettings.getPrecipCaution1Hr() != currentMenuSettings.getPrecipCaution1Hr()) ||
                    (_prevMenuSettings.getPrecipCaution3Hr() != currentMenuSettings.getPrecipCaution3Hr()) ||
                    (_prevMenuSettings.getPrecipCaution6Hr() != currentMenuSettings.getPrecipCaution6Hr()) ||
                    (_prevMenuSettings.getRatioCaution1Hr() != currentMenuSettings.getRatioCaution1Hr()) ||
                    (_prevMenuSettings.getRatioCaution3Hr() != currentMenuSettings.getRatioCaution3Hr()) ||
                    (_prevMenuSettings.getRatioCaution6Hr() != currentMenuSettings.getRatioCaution6Hr()) ||
                    (_prevMenuSettings.getDiffCaution1Hr() != currentMenuSettings.getDiffCaution1Hr()) ||
                    (_prevMenuSettings.getDiffCaution3Hr() != currentMenuSettings.getDiffCaution3Hr()) ||
                    (_prevMenuSettings.getDiffCaution6Hr() != currentMenuSettings.getDiffCaution6Hr()))
            {
                _prevMenuSettings = currentMenuSettings;
                _savedMenuSettings = currentMenuSettings;
                _logger.log("PrecipThresholdDialog.ApplyMenuSettingsListener : New precip settings applied");
             }
        }
    }

    private class ClosePrecipSettingsDialogListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            closePrecipSettingsDialog();
        }
    }

}

