package ohd.hseb.monitor.river;

import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;

import ohd.hseb.monitor.MonitorFrame;
import ohd.hseb.monitor.river.settings.RiverColumnDataSettings;
import ohd.hseb.util.gui.WindowResizingManager;

public class LookBackTimeDialog extends JDialog
{
	private JSpinner _timeSpinBox;
	private JLabel _validTimeLabel;
	private JButton _closeButton;
	private String _settingsTokenName;
	private RiverColumnDataSettings _riverSettings;
	
	public LookBackTimeDialog( MonitorFrame mainFrame, RiverColumnDataSettings riverSettings,
			String dialogTitle, 
			JLabel label,
			String settingsTokenName,
			int maxNumOfHrs,
			int minNumOfHrs)
	{
		super(mainFrame, true);
		_settingsTokenName = settingsTokenName;
		_riverSettings = riverSettings;
		this.setTitle(dialogTitle);
    	createDialogComponents(label, maxNumOfHrs, minNumOfHrs);
    	this.pack();
    	this.setVisible(true);
	}
	
	public void createDialogComponents(JLabel label, int maxNumOfHrs, int minNumOfHrs)
	{
		_validTimeLabel = label;
		int incrNumOfHrs = 1;
		int initialNumOfHrs = 1;

		if(_settingsTokenName.equals(RiverColumnDataSettings.VALID_TIME_TAG))
		   initialNumOfHrs = _riverSettings.getAlertAlarmLookBack();
		else if(_settingsTokenName.equals(RiverColumnDataSettings.VTEC_EVENT_PRODUCT_TIME_TAG))
            initialNumOfHrs = _riverSettings.getVtecEventProductTimeLookBack();
		else if(_settingsTokenName.equals(RiverColumnDataSettings.VTEC_EVENT_END_TIME_TAG))
			initialNumOfHrs = _riverSettings.getVtecEventEndTimeApproach();
		else if(_settingsTokenName.equals(RiverColumnDataSettings.UGC_EXPIRE_TIME_TAG))
			initialNumOfHrs = _riverSettings.getUgcExpireTimeApproach();
        else if(_settingsTokenName.equals(RiverColumnDataSettings.LATEST_FCST_BASIS_TIME_TAG))
            initialNumOfHrs = _riverSettings.getLatestFcstBasisTimeLookBack();
		else // LatestObs_Time
			initialNumOfHrs = _riverSettings.getLatestObsLookBack();
		
		SpinnerModel spinnerModel = new SpinnerNumberModel(initialNumOfHrs,
				minNumOfHrs,
				maxNumOfHrs,
				incrNumOfHrs);
		
    	_timeSpinBox = new JSpinner(spinnerModel);
    	JPanel spinnerPanel = new JPanel();
		spinnerPanel.setLayout(new GridLayout(3,1));
		spinnerPanel.add(createDummyPanel());
		spinnerPanel.add(_timeSpinBox);
		spinnerPanel.add(createDummyPanel());

    	JPanel infoPanel = new JPanel();
    	infoPanel.setLayout(new GridLayout(1,3));
    	infoPanel.add(_validTimeLabel);
    	infoPanel.add(spinnerPanel);
    	infoPanel.add(createDummyPanel());
    	
    	_closeButton = new JButton("Close");
    	JPanel closePanel = new JPanel();
    	closePanel.setLayout(new GridLayout(3,1));
        closePanel.add(createDummyPanel());
        closePanel.add(_closeButton);
        closePanel.add(createDummyPanel());
        
    	JPanel actionPanel = new JPanel();
    	actionPanel.setLayout(new GridLayout(1,3));
    	actionPanel.add(createDummyPanel());
    	actionPanel.add(closePanel);
    	actionPanel.add(createDummyPanel());
    	
    	JPanel panel = new JPanel();
    	panel.setLayout(new GridLayout(2,1));
    	panel.add(infoPanel);
    	panel.add(actionPanel);
    	
    	this.setLocation(25, 25);
    	Dimension dim = new Dimension(300,200);
    	new WindowResizingManager(this, dim, dim);
    	this.getContentPane().add(panel);
    	
    	CloseButtonListener closeButtonListener = new CloseButtonListener();
    	_closeButton.addActionListener(closeButtonListener);
    	
    	DialogListener dialogListener = new DialogListener();
    	this.addWindowListener(dialogListener);
 	}
	
	private JPanel createDummyPanel()
	{
		JPanel dummyPanel = new JPanel();
		return dummyPanel;
	}
	
	private void closeTimeDialog()
	{
		this.setVisible(false);
	
		if(_settingsTokenName.equals(RiverColumnDataSettings.VALID_TIME_TAG))
		 _riverSettings.setAlertAlarmLookBack(Integer.parseInt(_timeSpinBox.getValue().toString()));
		else if(_settingsTokenName.equals(RiverColumnDataSettings.VTEC_EVENT_PRODUCT_TIME_TAG))
            _riverSettings.setVtecEventProductTimeLookBack(Integer.parseInt(_timeSpinBox.getValue().toString()));
		else if(_settingsTokenName.equals(RiverColumnDataSettings.VTEC_EVENT_END_TIME_TAG))
		    _riverSettings.setVtecEventEndTimeApproach(Integer.parseInt(_timeSpinBox.getValue().toString()));
		else if(_settingsTokenName.equals(RiverColumnDataSettings.UGC_EXPIRE_TIME_TAG))
		    _riverSettings.setUgcExpireTimeApproach(Integer.parseInt(_timeSpinBox.getValue().toString()));
        else if(_settingsTokenName.equals(RiverColumnDataSettings.LATEST_FCST_BASIS_TIME_TAG))
            _riverSettings.setLatestFcstBasisTimeLookBack(Integer.parseInt(_timeSpinBox.getValue().toString()));
		else // LatestObs_Time
		    _riverSettings.setLatestObsLookBack(Integer.parseInt(_timeSpinBox.getValue().toString()));
	}
	
	class CloseButtonListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e) 
		{
			closeTimeDialog();
		}
	}
	
	class DialogListener extends WindowAdapter
	{
		public void windowClosing(WindowEvent e)
		{
			closeTimeDialog();
		}
	}
}
