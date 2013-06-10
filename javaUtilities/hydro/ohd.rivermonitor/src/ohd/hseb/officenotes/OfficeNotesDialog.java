package ohd.hseb.officenotes;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.InputVerifier;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.text.JTextComponent;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.ihfsdb.generated.OfficeNotesRecord;
import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DateTimeTextField;
import ohd.hseb.util.gui.WindowResizingManager;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableManager;

public class OfficeNotesDialog extends JDialog
{
    private List _officeNotesColumnDescriptorList = null;
    private JTableManager _officeNotesTableManager = null;
    private OfficeNotesDataManager _officeNotesDataMgr = null;
    private List _allRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JFrame _mainFrame;
    private String _id;
    private Map _lidDescDetailsMap;
    private String _definedIds[];
    private int _selectedRowIndex;
    private boolean _isUpdateOrDeleteToBeDone = false;

    private JLabel _topicLabel;
    private JTextField _topicField ;
    private JLabel _idLabel;
    private JLabel _nameLabel;
    private JLabel _countyLabel;
    private JLabel _stateLabel;
    private JTextField _nameField;
    private JTextField _countyField;
    private JTextField _stateField;
    private JComboBox _idComboBox;
    private JTextComponent _idComboBoxTextComponent;
    private JLabel _noteLabel ;
    private JTextArea _noteArea;
    private JLabel _dataTimeLabel ;
    private JTextField  _dataTimeField;
    private JLabel _postingTimeLabel ;
    private JTextField  _postingTimeField ;
    private JLabel _updateTimeLabel ;
    private JTextField  _updateTimeField ;
    private JLabel _expireTimeLabel;
    private JTextField  _expireTimeField ;
    private JButton _updateButton;
    private JButton _deleteButton ;
    private JButton _closeButton ;
    private String _missingRepresentation;

    private boolean _infoDialogShown = false;
    private boolean _displayingSelectedRowData = false;
    private SessionLogger _logger = null;

    private OfficeNotesJTableRowData _selectedRowData;
    private TextFieldInputVerifier _textFieldInputVerifier = new TextFieldInputVerifier();

    private int _previousRowToHighlight = -1;

    public OfficeNotesDialog(JFrame mainFrame, OfficeNotesDataManager officeNotesDataMgr, String topic, String[] ids, Map lidDescDetailsMap, SessionLogger logger)
    {
        super(mainFrame, true);
        _logger = logger;
        _mainFrame = mainFrame;
        _lidDescDetailsMap = lidDescDetailsMap;
        _definedIds = ids;
        this.setTitle("OfficeNotes");
        this.getContentPane().setLayout(new GridBagLayout());
        _officeNotesDataMgr = officeNotesDataMgr;
        initialize(officeNotesDataMgr, topic, ids);
        Dimension min = new Dimension(1100,570);
        new WindowResizingManager(this, min, min);
        this.pack();
        this.setLocation(25, 25);
    }

    public void showOfficeNotesDialog(String id)
    {
        if(id != null)
        {
            _idComboBox.setSelectedItem(id);
            _officeNotesTableManager.deselectRows(_previousRowToHighlight, _previousRowToHighlight);
            int rowToHighLight = _officeNotesTableManager.getTheRowIndexToHighLightBasedOnValue(id);
            if(rowToHighLight != -1)
            {
                _officeNotesTableManager.selectRows(rowToHighLight, rowToHighLight);
                _previousRowToHighlight = rowToHighLight;
            }
        }
        this.setVisible(true);
    }

    private void initialize(OfficeNotesDataManager officeNotesDataMgr, String topic, String[] ids)
    {
        _officeNotesColumnDescriptorList = new OfficeNotesColumns().getOfficeNotesColumnsList(_officeNotesColumnDescriptorList);
        _allRowDataList = officeNotesDataMgr.readDataFromOfficeNotes();
        _officeNotesTableManager = new ComplexJTableManager(_officeNotesColumnDescriptorList, _allRowDataList);
        String columnsSelected[] = _officeNotesTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _officeNotesTableManager.setDisplayableColumns(columnsSelected, true, true);
        _officeNotesTableManager.setPreferredSizeForJScrollPane(new Dimension(1080, 300));
        _officeNotesTableManager.setVerifyInputWhenFocusTarget(false);
        OfficeNotesTableMouseListener tableMouseListener = new OfficeNotesTableMouseListener();
        _officeNotesTableManager.addTableListener(tableMouseListener);
        _tableScrollPane = _officeNotesTableManager.getJScrollPane();

        JPanel tableScrollPanePanel = new JPanel(); 
        tableScrollPanePanel.add(_tableScrollPane);
        tableScrollPanePanel.setMinimumSize(new Dimension(1090,310));

        _topicLabel = new JLabel("Topic");
        _topicField = new JTextField(25);
        _topicField.setInputVerifier(_textFieldInputVerifier);
        if(topic != null)
        {
            _topicField.setText(topic);
        }

        _idLabel = new JLabel("Id");
        _idLabel.setMaximumSize(_topicLabel.getPreferredSize());
        if(ids == null)
        {
            _idComboBox = new JComboBox();
        }
        else
        {
            _idComboBox = new JComboBox(ids);
        }
        _idComboBoxTextComponent = (JTextComponent) _idComboBox.getEditor().getEditorComponent();
        _idComboBoxTextComponent.setInputVerifier(_textFieldInputVerifier);
        _idComboBox.setEditable(true);

        _nameLabel = new JLabel("      Name");
        _countyLabel = new JLabel("      County");
        _stateLabel = new JLabel("      State");
        _nameField = new JTextField();
        _countyField = new JTextField();
        _stateField = new JTextField();
        _nameField.setEditable(false);
        _countyField.setEditable(false);
        _stateField.setEditable(false);

        _noteLabel = new JLabel("Note");
        _noteArea = new JTextArea();
        _noteArea.setLineWrap(true);
        JScrollPane textPane = new JScrollPane(_noteArea);

        _dataTimeLabel = new JLabel("Data Time");
        _dataTimeField = new DateTimeTextField(System.currentTimeMillis(), _mainFrame, "Data Time", 20);
        _dataTimeField.setInputVerifier(_textFieldInputVerifier);

        _postingTimeLabel = new JLabel("Posting Time");
        _postingTimeField = new JTextField();
        _postingTimeField.setText(DbTimeHelper.getDateTimeStringFromLongTime(System.currentTimeMillis()));
        _postingTimeField.setEditable(false);

        _updateTimeLabel = new JLabel("Update Time");
        _updateTimeField = new JTextField();
        _updateTimeField.setText(DbTimeHelper.getDateTimeStringFromLongTime(System.currentTimeMillis()));
        _updateTimeField.setEditable(false);

        _expireTimeLabel = new JLabel("Expire Time");
        _expireTimeField = new DateTimeTextField(System.currentTimeMillis(), _mainFrame, "Expire Time", 20);
        _expireTimeField.setPreferredSize(new Dimension(100,30));
        _expireTimeField.setInputVerifier(_textFieldInputVerifier);

        JPanel idPanel = new JPanel();
        idPanel.setLayout(new GridLayout(1,2));
        idPanel.add(_idLabel);
        idPanel.add(_idComboBox);

        JPanel nameLabelPanel = new JPanel();
        nameLabelPanel.setLayout(new GridLayout(1,1));
        nameLabelPanel.add(_nameLabel);

        JPanel nameFieldPanel = new JPanel();
        nameFieldPanel.setLayout(new GridLayout(1,1));
        nameFieldPanel.add(_nameField);

        JPanel countyPanel = new JPanel();
        countyPanel.setLayout(new GridLayout(1,2));
        countyPanel.add(_countyLabel);
        countyPanel.add(_countyField);

        JPanel statePanel = new JPanel();
        statePanel.setLayout(new GridLayout(1,2));
        statePanel.add(_stateLabel);
        statePanel.add(_stateField);

        JPanel idInfoCountyInfoStateInfoPanel = new JPanel();
        idInfoCountyInfoStateInfoPanel.setLayout(new GridLayout(1,3));
        idInfoCountyInfoStateInfoPanel.add(idPanel);
        idInfoCountyInfoStateInfoPanel.add(countyPanel);
        idInfoCountyInfoStateInfoPanel.add(statePanel);

        JPanel topicPanel = new JPanel();
        topicPanel.setLayout(new GridLayout(1,3));
        topicPanel.add(_topicLabel);
        topicPanel.add(_topicField);
        topicPanel.add(_nameLabel);

        JPanel topicNamePanel = new JPanel();
        topicNamePanel.setLayout(new GridLayout(1,2));
        topicNamePanel.add(topicPanel);
        topicNamePanel.add(nameFieldPanel);

        JPanel noteLabelPanel = new JPanel();
        noteLabelPanel.setLayout(new GridLayout(1,1));
        noteLabelPanel.add(_noteLabel);

        JPanel topicInfoIdInfoNoteLabelPanel = new JPanel();
        topicInfoIdInfoNoteLabelPanel.setLayout(new GridLayout(3,1));
        topicInfoIdInfoNoteLabelPanel.add(topicNamePanel);
        topicInfoIdInfoNoteLabelPanel.add(idInfoCountyInfoStateInfoPanel);
        topicInfoIdInfoNoteLabelPanel.add(noteLabelPanel);

        JPanel noteFieldPanel = new JPanel();
        noteFieldPanel.setLayout(new GridLayout(1,1));
        noteFieldPanel.add(textPane);

        JPanel topicIdNotePanel = new JPanel();
        topicIdNotePanel.setLayout(new GridLayout(2,1));
        topicIdNotePanel.add(topicInfoIdInfoNoteLabelPanel);
        topicIdNotePanel.add(noteFieldPanel);

        JPanel infoPanel = new JPanel();
        infoPanel.setLayout(new GridLayout(1,1));
        infoPanel.add(topicIdNotePanel);

        JPanel timePanel = new JPanel();
        timePanel.setLayout(new GridLayout(4,3));
        timePanel.add(createDummyPanel());
        timePanel.add(_dataTimeLabel);
        _dataTimeField.setColumns(20);
        timePanel.add(_dataTimeField);
        timePanel.add(createDummyPanel());
        timePanel.add(_expireTimeLabel);
        _expireTimeField.setColumns(20);
        timePanel.add(_expireTimeField);

        timePanel.add(createDummyPanel());
        timePanel.add(_postingTimeLabel);
        _postingTimeField.setColumns(20);
        timePanel.add(_postingTimeField);
        timePanel.add(createDummyPanel());
        timePanel.add(_updateTimeLabel);
        _updateTimeField.setColumns(20);
        timePanel.add(_updateTimeField);

        JPanel actionPanel = new JPanel();
        actionPanel = new JPanel();
        _updateButton = new JButton("Save");
        _deleteButton = new JButton("Delete");
        _closeButton = new JButton("Close");
        _closeButton.setVerifyInputWhenFocusTarget(false);
        actionPanel.add(_updateButton);
        actionPanel.add(_deleteButton);
        actionPanel.add(_closeButton);

        JPanel editPanel = new JPanel();
        editPanel.setLayout(new GridLayout(1,2));
        editPanel.setBorder(BorderFactory.createTitledBorder("Details")); 

        editPanel.add(infoPanel);
        editPanel.add(timePanel);

        Container contentPane = this.getContentPane();
        ComponentHelper.addFrameComponent(contentPane,  tableScrollPanePanel, 0, 0,1,1,1, 1, 1);
        ComponentHelper.addFrameComponent(contentPane, editPanel, 0, 1,1,1, 1, 1, 1);
        ComponentHelper.addFrameComponent(contentPane, actionPanel, 0, 2,1,1, 1, 1, 0);

        IdListener idListener = new IdListener();
        _idComboBox.addActionListener(idListener);

        UpdateListener updateListener = new UpdateListener();
        _updateButton.addActionListener(updateListener);

        DeleteListener deleteListener = new DeleteListener();
        _deleteButton.addActionListener(deleteListener);

        CloseListener closeListener = new CloseListener();
        _closeButton.addActionListener(closeListener);

        _idComboBox.setSelectedIndex(0);
    }

    private JPanel createDummyPanel()
    {
        JPanel dummyPanel = new JPanel();
        return dummyPanel;
    }

    private void closeOfficeNotesDialog()
    {
        this.setVisible(false);
    }

    private void showCurrentRowData(String id)
    {
        if(_selectedRowData != null)
        {
            _displayingSelectedRowData = true;
            _topicField.setText(_selectedRowData.getTopic());
            _idComboBox.setSelectedItem(_selectedRowData.getId());
            _noteArea.setText(_selectedRowData.getNote());

            String timeStr = DbTimeHelper.getDateTimeStringFromLongTime(_selectedRowData.getPostingTime());
            _postingTimeField.setText(timeStr);

            if(! _selectedRowData.getDataValue(OfficeNotesColumns.UPDATE_TIME).equals(_missingRepresentation))
            {
                timeStr = DbTimeHelper.getDateTimeStringFromLongTime(_selectedRowData.getUpdateTime());
                _updateTimeField.setText(timeStr);
            }
            else
                _updateTimeField.setText("yyyy-mm-dd hh:mm:ss");

            if(! _selectedRowData.getDataValue(OfficeNotesColumns.DATA_TIME).equals(_missingRepresentation))
            {
                timeStr = DbTimeHelper.getDateTimeStringFromLongTime(_selectedRowData.getDataTime());
                _dataTimeField.setText(timeStr);
            }
            else
                _dataTimeField.setText("yyyy-mm-dd hh:mm:ss");

            if(!_selectedRowData.getDataValue(OfficeNotesColumns.EXPIRE_TIME).equals(_missingRepresentation))
            {
                timeStr = DbTimeHelper.getDateTimeStringFromLongTime(_selectedRowData.getExpireTime());
                _expireTimeField.setText(timeStr);
            }
            else
                _expireTimeField.setText("yyyy-mm-dd hh:mm:ss");
            _displayingSelectedRowData = false;
        }
        else if(id == null)
        {
            String str="";
            _topicField.setText(str);
            _idComboBox.setSelectedItem(_idComboBox.getSelectedItem().toString());
            _noteArea.setText(str);
            _postingTimeField.setText(DbTimeHelper.getDateTimeStringFromLongTime(System.currentTimeMillis()));
            _updateTimeField.setText(_postingTimeField.getText());
            _dataTimeField.setText("yyyy-mm-dd hh:mm:ss");
            _expireTimeField.setText("yyyy-mm-dd hh:mm:ss");
        }
        else // id is not null
        {
            _topicField.setText(_topicField.getText());
            _idComboBox.setSelectedItem(id);
            _noteArea.setText(_noteArea.getText());
            _postingTimeField.setText(_postingTimeField.getText());
            _updateTimeField.setText(_updateTimeField.getText());
            _dataTimeField.setText(_dataTimeField.getText());
            _expireTimeField.setText(_expireTimeField.getText());
        }
    }

    private OfficeNotesJTableRowData getMatchingRecordIfIdExistsInTable(String id)
    {
        OfficeNotesJTableRowData rowData = null;
        for(int i=0; i < _allRowDataList.size(); i++)
        {
            OfficeNotesJTableRowData tempRec = (OfficeNotesJTableRowData) _allRowDataList.get(i);
            if(tempRec.getId().equals(id))
            {
                rowData = tempRec;
                break;
            }
        }
        return rowData;
    }

    private class CloseListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            closeOfficeNotesDialog();            
        }
    }

    private OfficeNotesRecord getOfficeNotesRecord()
    {
        OfficeNotesRecord record = new OfficeNotesRecord();
        if(!(_topicField.getInputVerifier().verify(_topicField)))
        {
            JOptionPane.showMessageDialog(null, "Topic length must be >=1 and <= 8","OfficeNotes Application", JOptionPane.PLAIN_MESSAGE);
            _infoDialogShown = true;
            return null;
        }
        else
        {
            record.setTopic(_topicField.getText());
        }

        if(!(_idComboBoxTextComponent.getInputVerifier().verify(_idComboBoxTextComponent)))
        {
            JOptionPane.showMessageDialog(null, "Id length must be >=1 and <= 8","OfficeNotes Application", JOptionPane.PLAIN_MESSAGE);
            _infoDialogShown = true;
            return null;
        }
        else
        {
            record.setId(_idComboBox.getSelectedItem().toString());
        }

        if(!(_dataTimeField.getInputVerifier().verify(_dataTimeField)))
        {
            JOptionPane.showMessageDialog(null, "Enter valid data time","OfficeNotes Application", JOptionPane.PLAIN_MESSAGE);
            _infoDialogShown = true;
            return null;
        }
        else
        {
            record.setDatatime(DbTimeHelper.getLongTimeFromDateTimeString(_dataTimeField.getText()));
        }
        if(!(_expireTimeField.getInputVerifier().verify(_expireTimeField)))
        {
            JOptionPane.showMessageDialog(null, "Enter valid expire time","OfficeNotes Application", JOptionPane.PLAIN_MESSAGE);
            _infoDialogShown = true;
            return null;
        }
        else
        {
            long dataTime = DbTimeHelper.getLongTimeFromDateTimeString(_dataTimeField.getText());
            long expireTime = DbTimeHelper.getLongTimeFromDateTimeString(_expireTimeField.getText());
            if(dataTime > expireTime)
            {
                JOptionPane.showMessageDialog(null, "Expire Time should be Greater than Data Time","OfficeNotes Application", JOptionPane.PLAIN_MESSAGE);
                _infoDialogShown = true;
                return null;
            }
            else
            {
                record.setExpiretime(DbTimeHelper.getLongTimeFromDateTimeString(_expireTimeField.getText()));
                _infoDialogShown = false;
            }
        }
        record.setPostingtime(DbTimeHelper.getLongTimeFromDateTimeString(_postingTimeField.getText()));
        record.setUpdatetime(DbTimeHelper.getLongTimeFromDateTimeString(_updateTimeField.getText()));
        String note = _noteArea.getText();
        if(note.length() > 510)
            note = note.substring(0, 510);
        record.setNote(note);
        return record;
    }

    private class TextFieldInputVerifier extends InputVerifier
    {
        public boolean verify(JComponent component) 
        {
            boolean returnValue = true;

            if(!_isUpdateOrDeleteToBeDone)
                return returnValue;

            JTextField textField = (JTextField)component;
            if(component == _topicField)
            {
                String topic= textField.getText();
                if(topic != null)
                {
                    if(topic.length() == 0)
                        returnValue = false;
                    else if(topic.length() > 8)
                        returnValue = false;
                }
                else
                    returnValue = false;
            }
            else if(component == _idComboBoxTextComponent)
            {
                String id= textField.getText();
                if(id != null)
                {
                    if(id.length() == 0)
                        returnValue = false;
                    else if(id.length() > 8)
                        returnValue = false;
                }
                else
                    returnValue = false;
            }
            else if(component == _dataTimeField)
            {
                String dataTime = textField.getText();
                if(dataTime != null)
                {
                    if(_dataTimeField.getText().equals("yyyy-mm-dd hh:mm:ss"))
                        returnValue = false;
                }
                else
                    returnValue = false;
            }
            else if(component == _expireTimeField)
            {
                String expireTime = textField.getText();
                if(expireTime != null)
                {
                    if(_expireTimeField.getText().equals("yyyy-mm-dd hh:mm:ss"))
                        returnValue = false;
                }
                else
                    returnValue = false;
            }
            return returnValue;
        }
    }

    private void getCurrentDataFromDataMgr()
    {
        List dbInfoList = null;
        String header = "OfficeNotesDialog.getCurrentDataFromDataMgr() ";
        try
        {
            dbInfoList = _officeNotesDataMgr.readDataFromOfficeNotes();
        }
        catch(Exception e)
        {
            _logger.log(header+ e);
        }
        _allRowDataList = dbInfoList;
        _officeNotesTableManager.setChangedAllRowDataList(_allRowDataList);
    }

    class IdListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            _id = _idComboBox.getSelectedItem().toString();
            boolean isDefinedId = false;
            for(int i=0; i < _definedIds.length; i++)
            {
                if(_definedIds[i].equals(_id))
                {
                    isDefinedId = true;
                    break;
                }
            }
            if(isDefinedId)
            {
                String desc = (String)_lidDescDetailsMap.get(_id);
                if(desc != null)
                {
                    String splitStr[] = desc.split(";");
                    _nameField.setText(splitStr[0]);
                    _countyField.setText(splitStr[2]);
                    _stateField.setText(splitStr[1]);
                }
            }
            else
            {
                _nameField.setText("");
                _countyField.setText("");
                _stateField.setText("");
            }

            if(!_displayingSelectedRowData)
            {
                _selectedRowData = null;
                showCurrentRowData(_id);
            }
        }
    }

    class DeleteListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            _isUpdateOrDeleteToBeDone = true;
            String header = "OfficeNotesDialog.DeleteListener.actionPerformed(): ";
            OfficeNotesRecord record = getOfficeNotesRecord();
            if(!_infoDialogShown)
            {
                _logger.log(header+" Delete record"+ record.toString());
                _officeNotesDataMgr.delete(record);
                getCurrentDataFromDataMgr();
                _officeNotesTableManager.refreshDisplay();
                _selectedRowData = null;
                showCurrentRowData(_idComboBox.getSelectedItem().toString());
            }
            _isUpdateOrDeleteToBeDone = false;

        }
    }

    class UpdateListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            _isUpdateOrDeleteToBeDone = true;
            String header = "OfficeNotesDialog.UpdateListener.actionPerformed(): ";
            if(getMatchingRecordIfIdExistsInTable(_idComboBox.getSelectedItem().toString()) == null)
            {
                _postingTimeField.setText(DbTimeHelper.getDateTimeStringFromLongTime(System.currentTimeMillis()));
                _updateTimeField.setText(_postingTimeField.getText());
            }
            else
            {
                _updateTimeField.setText(DbTimeHelper.getDateTimeStringFromLongTime(System.currentTimeMillis()));
            }
            OfficeNotesRecord record = getOfficeNotesRecord();
            if(!_infoDialogShown)
            {
                _logger.log(header+" Update record"+ record.toString());
                _officeNotesDataMgr.updateOrInsert(record);
                getCurrentDataFromDataMgr();
                _officeNotesTableManager.refreshDisplay();
                int rowToHighLight = _officeNotesTableManager.getTheRowIndexToHighLightBasedOnValue(_idComboBox.getSelectedItem().toString());
                _officeNotesTableManager.selectRows(rowToHighLight, rowToHighLight);
            }
            _isUpdateOrDeleteToBeDone = false;
        }

    }

    class OfficeNotesTableMouseListener implements MouseListener
    {
        public void mouseClicked(MouseEvent e) 
        {
            Point p = new Point(e.getX(), e.getY());
            _selectedRowIndex = _officeNotesTableManager.getMousePointedRowIndex(p);
            _selectedRowData = (OfficeNotesJTableRowData) _officeNotesTableManager.getSelectedRowData(_selectedRowIndex);
            showCurrentRowData(null);
        }
        public void mousePressed(MouseEvent e){}
        public void mouseReleased(MouseEvent e){}
        public void mouseEntered(MouseEvent e){}
        public void mouseExited(MouseEvent e){}
    }

}
