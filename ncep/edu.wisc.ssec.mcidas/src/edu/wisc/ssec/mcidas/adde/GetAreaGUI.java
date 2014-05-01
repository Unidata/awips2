//
// GetAreaGUI.java
//

/*
This source file is part of the edu.wisc.ssec.mcidas package and is
Copyright (C) 1998 - 2009 by Tom Whittaker, Tommy Jasmin, Tom Rink,
Don Murray, James Kelly, Bill Hibbard, Dave Glowacki, Curtis Rueden
and others.
 
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA
*/

package edu.wisc.ssec.mcidas.adde;
// Created on August 29, 2000, 8:45 AM

import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.text.*;
import java.io.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import java.util.Properties;
import java.util.Vector;
import edu.wisc.ssec.mcidas.*;
import edu.wisc.ssec.mcidas.AreaDirectory;
import edu.wisc.ssec.mcidas.adde.*;
import edu.wisc.ssec.mcidas.adde.AddeServerInfo;

/**
 * A GUI wrapper for whatever class/method tries to
 * get image data via ADDE, but needs a way to let the
 * user explore the availability of data.
 *
 * Possible defaults are written into the GetAreaGUI.properties
 * file, and when the user indicates "use my defaults", these are
 * employed when ever the correct combo of server/group/descr are
 * chosen.  
 *
 * @author tomw
 * @version 0.1
 */
public class GetAreaGUI extends JPanel {
  AddeServerInfo asi;
  String[] sl;
  String selectedServer, selectedGroup, selectedDescr, selectedDateTime;
  // note that 'descr' and 'dataset' are the same thing...
  
  String coordType;
  String actionButtonString;
  String userName, projectNumber;
  boolean serverUpdated, groupUpdated, descrUpdated, multipleImages;
  ActionListener al = null;
  int baseNumLines, baseNumEles;
  double resLat, resLon;
  int[] bandListIndex;
  String[] bandList;
  String selectedBand, selectedUnit;
  int selectedBandIndex;
  String[][][] calInfo = null;
  boolean doingRes;
  NumberFormat nf3;
  Properties dataProp;
  StringBuffer serverList;
  AreaDirectory [][] ad;
  int areaIndex;
  String[] bandNames;
  AddeSatBands asb;
  boolean closeOnAction = true;
  boolean useDefaults = false;
  boolean gotUserDefaults = false;
  String propFile;
  String cmdout=null;
  ArrayList imageList = null;
  JDialog dialog;

  /** 
  * @param s is the label for the action button
  *
  */
  public GetAreaGUI(String s) {
    this ((Frame)null, false, s, false, true, true);
  }
  
  /** 
  * @param s is the label for the action button
  * @param multi is true if multiple selection mode is to be used 
  *  (this does not work yet!!)
  *
  */
  public GetAreaGUI(String s, boolean multi) {
    this((Frame) null, false, s, multi, true, true);
  }
  
  /** 
  * @param s is the label for the action button
  * @param multi is true if multiple selection mode is to be used 
  *  (this does not work yet!!)
  * @param coa is true if the Dialog should close the window after
  *  the actionPerformed is done.
  *
  */
  public GetAreaGUI(String s, boolean multi, boolean coa) {
    this((Frame) null, false, s, multi, coa, true);
  }

  /** 
  * @param s is the label for the action button
  * @param multi is true if multiple selection mode is to be used 
  *  (this does not work yet!!)
  * @param coa is true if the Dialog should close the window after
  *  the actionPerformed is done.
  * @param modal is true if this should be a modal dialog
  *
  */
  public GetAreaGUI(String s, boolean multi, boolean coa, boolean modal) {
    this((Frame) null, modal, s, multi, coa, true);
  }
  /** 
  * @param owner is the top-level Frame that owns this
  * @param modal is true if this should be a modal dialog
  * @param s is the label for the action button
  * @param multi is true if multiple selection mode is to be used 
  *  (this does not work yet!!)
  * @param coa is true if the Dialog should close the window after
  *  the actionPerformed is done.
  *
  */
  public GetAreaGUI(Frame owner, boolean modal, String s, boolean multi,
                    boolean coa) {
    this(owner, modal, s, multi, coa, true);
  }

  /** 
  * @param owner is the top-level Frame that owns this
  * @param modal is true if this should be a modal dialog
  * @param s is the label for the action button
  * @param multi is true if multiple selection mode is to be used 
  *  (this does not work yet!!)
  * @param coa is true if the Dialog should close the window after
  *  the actionPerformed is done.
  * @param dodialog is true if this should pop up a Dialog interface
  * box.  If this is false, then owner and modal may be null.
  *
  */
  public GetAreaGUI(Frame owner, boolean modal, String s, boolean multi, boolean coa, boolean dodialog) {

    setupGUI(s, multi, coa, owner, modal, dodialog);
  }
    
  private void setupGUI(String s, boolean multi, boolean coa, Frame parent, boolean modal, boolean dod) {

    serverUpdated = false;
    groupUpdated = false;
    descrUpdated = false;
    bandNames = null;
    closeOnAction = coa;
    selectedUnit = " ";
    selectedBand = " ";
    selectedBandIndex = -1;
    doingRes = false;
    asb = null;
    multipleImages = multi;
    nf3 = NumberFormat.getNumberInstance();
    nf3.setMaximumFractionDigits(3);
    actionButtonString = s;
    asi = new AddeServerInfo();
    String[] sla = asi.getServerList();
    dataProp = new Properties();
    try {
      String path = System.getProperty("user.home");
      propFile =
          path+System.getProperty("file.separator")+"GetAreaGUI.properties";
      FileInputStream fi = new FileInputStream(propFile);
      dataProp.load(fi);
      fi.close();
    } catch (Exception e) {;}
    
    String usl = (String) dataProp.get("user|server|list");
    serverList = new StringBuffer();
    ArrayList als = new ArrayList();
    for (int i=0; i<sla.length; i++) {
      als.add(sla[i]);
    }
    if (usl != null) {
      serverList.append(usl); 
      StringTokenizer slt = new StringTokenizer(usl,",");
      int n = slt.countTokens();
      for (int i=0; i<n; i++) {
        als.add(slt.nextToken());
      }
    }

    sl = new String[als.size()];
    for (int i=0; i<als.size(); i++) {
      sl[i] = (String) als.get(i);
    }
        
    selectedServer = null;
    selectedGroup = null;
    selectedDescr = null;
    selectedDateTime = null;
    
    baseNumLines = -1;
    baseNumEles = -1;
    
    initComponents ();
    buttGroupLoc = new javax.swing.ButtonGroup();
    buttGroupLoc.add(LatLonButton);
    buttGroupLoc.add(LinEleButton);
    buttGroupLoc.add(IDButton);
    
    dialog = null;
    if (dod) {
      dialog = new JDialog(parent, "ADDE Image Data Selector", modal);
      dialog.getContentPane().add(this);
      dialog.pack();
    }
  }

  public void show() {
    if (dialog != null) dialog.show();
  }

    /** define the name of the ADDE server to select
     * @param s - The ADDE server hostname.
     */
  public void setServer(String s) {
    selectedServer = s;
  }
    /** fetch the name of the currently selected ADDE server
     * @return The ADDE server (host) name
     */
  public String getServer() {
    return selectedServer;
  }
    /** force a particular ADDE Group to be selected.  If this is done, then
     * then you must also select the Dataset (aka Descr).
     * @param s The name of the ADDE data group
     */
  public void setGroup(String s) {
    selectedGroup = s;
  }
    /** fetch the name of the currently selected ADDE group
     * @return the ADDE group name (abreviation)
     */
  public String getGroup() {
    return selectedGroup;
  }
    /** force the selection of a particular ADDE Descr (dataset).  This will trigger
     * fetch from the server of available times/days.
     * @param s The name (abreviation) of the dataset (aka 'descr')
     */
  public void setDescr(String s) {
    selectedDescr = s;
  }
    /** fetch the name (abreviation) of the currently selected dataset (descr)
     * @return The name (abreviation) of the selected dataset/descr
     */
  public String getDescr() {
    return selectedDescr;
  }
  
  /** set the maginification (line element) factors
  * 
  */
  public void setMag(String m) {
    if (m != null) {
      StringTokenizer st = new StringTokenizer(m," ");
      int lm = Integer.parseInt(st.nextToken().trim());
      int em = Integer.parseInt(st.nextToken().trim());
      setLineMag(lm);
      setEleMag(em);
    } else {
      setLineMag(1);
      setEleMag(1);
    }
  }
  
  /** get the magnification factors (line element)
  *
  * @return string of line & element magnification, separated by a space
  */
  public String getMag() {
    return (LMagValue+" "+EMagValue);
    
  }
    /** define the value of the line maginfication (-50 thru +50).    Calling this also forces the Element Magnification
     * to be set to this value.  You may set it separately, as needed.
     * @param m The value of the line magnification (-50 thru +50).
     * Values of -1, 0, and 1 will be treated as 1.
     */
  public void setLineMag(int m) {
    LMagValue = m;
    if (LMagValue > -2 && LMagValue < 2) LMagValue = 1;
    LMagSlider.setValue(LMagValue+50);
    if (doingRes) {
      double v = resLat/LMagValue;
      if (LMagValue < 0) v = -(resLat * LMagValue);
      LMagLabel.setText("Line Resolution = "+nf3.format(v));
    } else {
      LMagLabel.setText("Line Magnification = "+LMagValue);
    }
    // need to set slider, too
  }
    /** fetch the current line magnification factor
     * @return the line magnification factor
     */
  public int getLinMag() {
    return LMagValue;
  }
  
    /** set the element magnification factor.  This call should be made after a call
     * to setLineMag()
     * @param m The element magnification factor (-50 thru +50).
     * Values of -1, 0, and 1 are treated as 1.
     */
  public void setEleMag(int m) {
    EMagValue = m;
    if (EMagValue > -2 && EMagValue < 2) EMagValue = 1;
    EMagSlider.setValue(EMagValue+50);
    if (doingRes) {
      double v = resLon/EMagValue;
      if (EMagValue < 0) v = -(resLon * EMagValue);
      EMagLabel.setText("Element Resolution = "+nf3.format(v));
    } else {
      EMagLabel.setText("Element Magnification = "+EMagValue);
    }
  }
  /** get the Element magnification
  *
  * @return element magnification factor
  */
  public int getEleMag() {
    return EMagValue;
  }
  
    /** define the coodinate type for the centered location values
     * @param c coordinate = "E" for earth, "I" for image, "S" for radar
     * station
     */
  public void setCoordType(String c) {
    LatLonButton.setSelected(false);
    LinEleButton.setSelected(false);
    IDButton.setSelected(false);
    if (c.equalsIgnoreCase("E")) LatLonButton.setSelected(true);
    if (c.equalsIgnoreCase("I")) LinEleButton.setSelected(true);
    if (c.equalsIgnoreCase("S")) IDButton.setSelected(true);
    setLocButtonLabel();
    // need to set slider, too
  }

    /** fetch the current coordinate type
     * @return coordinate type value ("E", "I", or "S")
     */
  public String getCoordType() {
    return coordType;
  }
  
    /** define the location(s) for the coodinate type.  It is assumed that you will
     * call setCoordType() and then setLocationString() with consistent values.
     * @param c The coordinate locations. If two values, separate by
     * one of more blanks.  For Earth coordinate, the latitude
     * and longitude (e.g., "43.1234 -89.2313"); for Image
     * coordinates, the line and element (e.g., "12345 23412"),
     * and for Radar Stations, the station name (e.g., "KMKX").
     */
  public void setLocationString(String c) {
    
    String locOne = " ";
    String locTwo = " ";
    if (c != null) {
      StringTokenizer st = new StringTokenizer(c," ");
      locOne = st.nextToken();
      
      if (st.hasMoreTokens() ) {
        locTwo = st.nextToken();
      }
    }
    LatLineText.setText(locOne);
    LonEleText.setText(locTwo);
  }
  
    /** fetch the current locatoin string
     * @return The value of the location string(s) as a single string
     * with one blank space between values (if more than one)
     */
  public String getLocationString() {
    String loc = LatLineText.getText() + " " + LonEleText.getText();
    return loc;
  }
  
    /** define the satellite band number to use
     * @param c the band number (as a String)
     */
  public void setBand(String c) {
  }
    /** fetch the current band number
     * @return the currently selected band number
     */
  public String getBand() {
    return selectedBand;
  }
  
    /** define the size of the image to get
     * @param c The number of lines and number of elements, in a string
     * with one or more blank spaces between (e.g., "480 640")
     *
     */
  public void setNumLinesEles(String c) {
    if (c == null || c.trim().length() < 3) {
      NumLinesText.setText(" ");
      NumElesText.setText(" ");
      baseNumLines = -1;
      baseNumEles = -1;
      return;
    }
    StringTokenizer st = new StringTokenizer(c," ");
    String lin = st.nextToken();
    String ele = st.nextToken();
    NumLinesText.setText(lin);
    NumElesText.setText(ele);
    baseNumLines = Integer.parseInt(lin.trim());
    baseNumEles = Integer.parseInt(ele.trim());
  }
  
    /** fetch the number of lines and elements defined
     * @return the number of lines and elements, as a String with one
     * blank between values (e.g., "480 640")
     */
  public String getNumLinesEles() {
    String s = NumLinesText.getText() + " " + NumElesText.getText();
    return s;
  }
  
  /* get the day of the index-th selected item
  */
  public String getDay(int index) {
    Object [] vals = DateTimeList.getSelectedValues();
    selectedDateTime = (String) vals[index];
    String day = null;
    if (selectedDateTime != null) {
      int i = selectedDateTime.indexOf("/");
      if (i > 0) {
        day = selectedDateTime.substring(0,i).trim();
      }
    }
    return day;
    
  }

  /** return the day of the selected image
  * @return the day in the format:  yyyy-mm-dd
  */
  public String getDay() {
    selectedDateTime = (String) DateTimeList.getSelectedValue();
    String day = null;
    if (selectedDateTime != null) {
      int i = selectedDateTime.indexOf("/");
      if (i > 0) {
        day = selectedDateTime.substring(0,i).trim();
      }
    }
    return day;
    
  }

  /** set the day
  *
  * @param d the day in the form:  yyyy-mm-dd
  */
  public void setDay (String d) {
    setDateTime(d,getTime());
  }
  
  /** get the time of the index-th selected image
  */
  public String getTime(int index) {
    Object [] vals = DateTimeList.getSelectedValues();
    selectedDateTime = (String) vals[index];
    String time = null;
    if (selectedDateTime != null) {
      int i = selectedDateTime.indexOf("/");
      if (i > 0) {
        time = selectedDateTime.substring(i+1).trim();
      }
    }
    return time;
  }

  /** get the time of the selected image
  *
  * @return the time in the format:  hh:mm:ss
  */
  public String getTime() {
    selectedDateTime = (String) DateTimeList.getSelectedValue();
    String time = null;
    if (selectedDateTime != null) {
      int i = selectedDateTime.indexOf("/");
      if (i > 0) {
        time = selectedDateTime.substring(i+1).trim();
      }
    }
    return time;
  }
  /** set the time 
  *
  * @param t is the time in the format:  hh:mm:ss
  */
  public void setTime(String t) {
    setDateTime(getDay(), t);
  }
  
  public void setDateTime(String d, String t) {
    selectedDateTime = d+" / "+t;
    return;
  }
  
    /** define the units of the data to get
     * @param c the name of the Units (e.g., "BRIT")
     */
  public void setUnit(String c) {
    //UnitText.setText(c);
  }
    /** fetch the name of the units defined
     * @return the name of the units
     */
  public String getUnit() {
    //return UnitText.getText();
    return selectedUnit;
  }
  
    /** define the calibration type to use (e.g., "VISSR")
     * @param c the calibration type
     */
  public void setCal (String c) {
    calText = c;
  }
    /** fetch the current Calibration type
     * @return the current calibration type
     */
  public String getCal() {
    return calText;
  }
  
    /** define whether the "documentation block" will be returned with the data.
     * @param v set to 'true' to return the history documentation.
     */
  public void setDoc (String v) {
    if (v != null && v.indexOf("1")>-1) {
//      DocBox.setSelected(true);
    } else {
      //DocBox.setSelected(false);
    }
    
  }
    /** fetch the state of the doc request switch
     * @return the state of the doc request switch
     */
  public String getDoc() {
//    if (DocBox.isSelected()) {
      return "1";
//    } else {
//      return null;
    //}
  }
  
    /** define a user name (required by some ADDE servers for accounting).  If given,
     * this will be included in all requests generated by GetAreaGUI.
     * @param c The user's ADDE identifier (e.g., "jack")
     */
  public void setUserName (String c) {
    userName = c;
  }
    /** fetch the currently defined user name
     * @return the currently defined user name
     */
  public String getUserName() {
    return userName;
  }
  
    /** define a project number, which is required by some ADDE servers
     * @param c The project number to use (e.g., "12345")
     */
  public void setProjectNumber (String c) {
    projectNumber = c;;
  }
    /** fetch the currently defined project number
     * @return the project number value
     */
  public String getProjectNumber() {
    return projectNumber;
  }
  
  
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the FormEditor.
     */
  private void initComponents() {//GEN-BEGIN:initComponents
            PanelSGD = new javax.swing.JPanel();
            PanelSG = new javax.swing.JPanel();
            PanelServer = new javax.swing.JPanel();
            ServerLabel = new javax.swing.JLabel();
            ServerSelector = new javax.swing.JComboBox();
            PanelGroup = new javax.swing.JPanel();
            jLabel2 = new javax.swing.JLabel();
            GroupSelector = new javax.swing.JComboBox();
            PanelDescr = new javax.swing.JPanel();
            jLabel3 = new javax.swing.JLabel();
            DescrSelector = new javax.swing.JComboBox();
            PanelListMag = new javax.swing.JPanel();
            PanelMag = new javax.swing.JPanel();
            jPanel13 = new javax.swing.JPanel();
            jPanel9 = new javax.swing.JPanel();
            LMagSlider = new javax.swing.JSlider();
            LMagLabel = new javax.swing.JLabel();
            jPanel10 = new javax.swing.JPanel();
            EMagLabel = new javax.swing.JLabel();
            EMagSlider = new javax.swing.JSlider();
            DateTimeLabel = new javax.swing.JLabel();
            PanelList = new javax.swing.JPanel();
            DateTimeScrollPanel = new javax.swing.JScrollPane();
            String [] prompt = {"Date-times of available","images will appear here"};
            DateTimeList =  new javax.swing.JList(prompt);
            LinesElesPanel = new javax.swing.JPanel();
            SizeLabel = new javax.swing.JLabel();
            jPanel7 = new javax.swing.JPanel();
            NumLinesLabel = new javax.swing.JLabel();
            NumLinesText = new javax.swing.JTextField();
            NumElesLabel = new javax.swing.JLabel();
            NumElesText = new javax.swing.JTextField();
            userDefaultsCheckBox = new javax.swing.JCheckBox();
            PanelBandUnit = new javax.swing.JPanel();
            BandPanel = new javax.swing.JPanel();
            BandLabel = new javax.swing.JLabel();
            BandBox = new javax.swing.JComboBox();
            UnitsPanel = new javax.swing.JPanel();
            UnitLabel = new javax.swing.JLabel();
            UnitBox = new javax.swing.JComboBox();
            UserActionPanel = new javax.swing.JPanel();
            userActionButton = new javax.swing.JButton();
            PanelStatus = new javax.swing.JPanel();
            statusLabel = new javax.swing.JTextField();
            PanelLoc = new javax.swing.JPanel();
            jPanel11 = new javax.swing.JPanel();
            PlaceLabel = new javax.swing.JLabel();
            LatLonButton = new javax.swing.JRadioButton();
            LinEleButton = new javax.swing.JRadioButton();
            IDButton = new javax.swing.JRadioButton();
            jPanel12 = new javax.swing.JPanel();
            LatLineLabel = new javax.swing.JLabel();
            LonEleLabel = new javax.swing.JLabel();
            LatLineText = new javax.swing.JTextField();
            LonEleText = new javax.swing.JTextField();
            
            setLayout(new java.awt.GridBagLayout());
            java.awt.GridBagConstraints gridBagConstraints1;
            
            setFont(new java.awt.Font("SansSerif", 0, 10));
            
            PanelSGD.setLayout(new java.awt.BorderLayout());
            
            PanelSGD.setMaximumSize(new java.awt.Dimension(500, 160));
            PanelServer.setLayout(new java.awt.GridLayout(2, 1));
            
            PanelServer.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
            PanelServer.setPreferredSize(new java.awt.Dimension(180, 45));
            ServerLabel.setText("Server");
            ServerLabel.setToolTipText("Select an ADDE Server from the list");
            ServerLabel.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
            PanelServer.add(ServerLabel);
            
            ServerSelector.setToolTipText("Select an ADDE Server from the list");
            ServerSelector.setBackground(java.awt.Color.lightGray);
            ServerSelector.setEditable(true);
            ServerSelector.setActionCommand("serverSelected");
            replaceList(ServerSelector, sl, "Select ADDE server");
            ServerSelector.setSelectedIndex(0);
            ServerSelector.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    ServerSelectorActionPerformed(evt);
                }
            });
            
            PanelServer.add(ServerSelector);
            
            PanelSG.add(PanelServer);
          
          PanelGroup.setLayout(new java.awt.GridLayout(2, 1));
            
            PanelGroup.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
            PanelGroup.setPreferredSize(new java.awt.Dimension(160, 45));
            jLabel2.setText("Dataset");
            jLabel2.setToolTipText("Select a dataset from the list");
            jLabel2.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
            PanelGroup.add(jLabel2);
            
            GroupSelector.setToolTipText("Select an ADDE data group from the list");
            GroupSelector.setBackground(java.awt.Color.lightGray);
            GroupSelector.setActionCommand("groupSelected");
            GroupSelector.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    GroupSelectorActionPerformed(evt);
                }
            });
            
            PanelGroup.add(GroupSelector);
            
            PanelSG.add(PanelGroup);
          
          PanelSGD.add(PanelSG, java.awt.BorderLayout.NORTH);
        
        PanelDescr.setLayout(new java.awt.BorderLayout());
          
          PanelDescr.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
          PanelDescr.setMaximumSize(new java.awt.Dimension(400, 50));
          jLabel3.setText("Data Type");
          jLabel3.setToolTipText("Select a Data Type; available dates and times will appear below");
          jLabel3.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
          PanelDescr.add(jLabel3, java.awt.BorderLayout.NORTH);
          
          DescrSelector.setToolTipText("Select an ADDE dataset; available dates and times will appear below");
          DescrSelector.setBackground(java.awt.Color.lightGray);
          DescrSelector.setName("descrType");
          DescrSelector.setActionCommand("descrSelected");
          DescrSelector.setMaximumSize(new java.awt.Dimension(500, 20));
          DescrSelector.addActionListener(new java.awt.event.ActionListener() {
              public void actionPerformed(java.awt.event.ActionEvent evt) {
                  DescrSelectorActionPerformed(evt);
              }
          });
          
          PanelDescr.add(DescrSelector, java.awt.BorderLayout.SOUTH);
          
          PanelSGD.add(PanelDescr, java.awt.BorderLayout.SOUTH);
        
        gridBagConstraints1 = new java.awt.GridBagConstraints();
              gridBagConstraints1.gridx = 0;
              gridBagConstraints1.gridy = 0;
              gridBagConstraints1.insets = new java.awt.Insets(2, 0, 2, 0);
              add(PanelSGD, gridBagConstraints1);
              
              PanelListMag.setLayout(new java.awt.GridBagLayout());
              java.awt.GridBagConstraints gridBagConstraints2;
              
              PanelListMag.setMinimumSize(new java.awt.Dimension(400, 150));
              PanelMag.setLayout(new java.awt.BorderLayout(10, 5));
              
              jPanel13.setLayout(new java.awt.BorderLayout(10, 0));
              
              jPanel13.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
              jPanel9.setLayout(new java.awt.BorderLayout());
              
              LMagSlider.setToolTipText("Slide to set line magnification factor");
              LMagSlider.addChangeListener(new javax.swing.event.ChangeListener() {
                  public void stateChanged(javax.swing.event.ChangeEvent evt) {
                      LMagSliderStateChanged(evt);
                  }
              });
              
              jPanel9.add(LMagSlider, java.awt.BorderLayout.SOUTH);
              
              LMagLabel.setText("Line Magnification");
              LMagLabel.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
              jPanel9.add(LMagLabel, java.awt.BorderLayout.NORTH);
              
              jPanel13.add(jPanel9, java.awt.BorderLayout.NORTH);
            
            jPanel10.setLayout(new java.awt.BorderLayout());
              
              EMagLabel.setText("Element Magnification");
              EMagLabel.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
              jPanel10.add(EMagLabel, java.awt.BorderLayout.NORTH);
              
              EMagSlider.setToolTipText("Slide to select element magnification factor");
              EMagSlider.addChangeListener(new javax.swing.event.ChangeListener() {
                  public void stateChanged(javax.swing.event.ChangeEvent evt) {
                      EMagSliderStateChanged(evt);
                  }
              });
              
              jPanel10.add(EMagSlider, java.awt.BorderLayout.SOUTH);
              
              jPanel13.add(jPanel10, java.awt.BorderLayout.SOUTH);
            
            PanelMag.add(jPanel13, java.awt.BorderLayout.NORTH);
          
          gridBagConstraints2 = new java.awt.GridBagConstraints();
        gridBagConstraints2.gridx = 0;
        gridBagConstraints2.gridy = 0;
        gridBagConstraints2.gridheight = 3;
        gridBagConstraints2.insets = new java.awt.Insets(5, 5, 5, 5);
        gridBagConstraints2.anchor = java.awt.GridBagConstraints.NORTH;
        PanelListMag.add(PanelMag, gridBagConstraints2);
        
        DateTimeLabel.setText("List of available Date / Times");
        gridBagConstraints2 = new java.awt.GridBagConstraints();
        gridBagConstraints2.gridx = 1;
        gridBagConstraints2.gridy = 0;
        gridBagConstraints2.anchor = java.awt.GridBagConstraints.SOUTH;
        PanelListMag.add(DateTimeLabel, gridBagConstraints2);
        
        PanelList.setLayout(new java.awt.BorderLayout(0, 10));
            
            PanelList.setPreferredSize(new java.awt.Dimension(180, 150));
            PanelList.setMinimumSize(new java.awt.Dimension(180, 150));
            DateTimeScrollPanel.setPreferredSize(new java.awt.Dimension(180, 150));
            DateTimeScrollPanel.setMinimumSize(new java.awt.Dimension(180, 150));
            DateTimeList.setToolTipText("Click on the date-time you want");
            DateTimeList.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
            DateTimeList.setName("");
            DateTimeList.setVisibleRowCount(10);
            if( multipleImages) {
              DateTimeList.setSelectionMode(javax.swing.ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
            } else {
              DateTimeList.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
            }
            DateTimeList.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
                public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
                    DateTimeListValueChanged(evt);
                }
            });
            
            DateTimeScrollPanel.setViewportView(DateTimeList);
            
            PanelList.add(DateTimeScrollPanel, java.awt.BorderLayout.CENTER);
          
          gridBagConstraints2 = new java.awt.GridBagConstraints();
        gridBagConstraints2.gridx = 1;
        gridBagConstraints2.gridy = 1;
        gridBagConstraints2.gridheight = 4;
        gridBagConstraints2.insets = new java.awt.Insets(0, 3, 0, 0);
        gridBagConstraints2.anchor = java.awt.GridBagConstraints.NORTH;
        PanelListMag.add(PanelList, gridBagConstraints2);
        
        LinesElesPanel.setLayout(new java.awt.BorderLayout());
          
          LinesElesPanel.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
          SizeLabel.setText("Size of Image to Get");
          SizeLabel.setToolTipText("Enter the number of lines and elements for the image you want (def=480 x 640)");
          SizeLabel.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
          LinesElesPanel.add(SizeLabel, java.awt.BorderLayout.NORTH);
          
          jPanel7.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 5, 0));
            
            NumLinesLabel.setText("Lines:");
            NumLinesLabel.setToolTipText("Enter the number of image lines you want");
            jPanel7.add(NumLinesLabel);
            
            NumLinesText.setToolTipText("Enter number of lines (def=480)");
            NumLinesText.setColumns(5);
            NumLinesText.setText(" ");
            jPanel7.add(NumLinesText);
            
            NumElesLabel.setText("       Elements:");
            NumElesLabel.setToolTipText("Enter the number of image Elements you want");
            jPanel7.add(NumElesLabel);
            
            NumElesText.setToolTipText("Enter number of elements (def=640)");
            NumElesText.setColumns(5);
            NumElesText.setText(" ");
            jPanel7.add(NumElesText);
            
            LinesElesPanel.add(jPanel7, java.awt.BorderLayout.SOUTH);
          
          gridBagConstraints2 = new java.awt.GridBagConstraints();
        gridBagConstraints2.gridx = 0;
        gridBagConstraints2.gridy = 3;
        gridBagConstraints2.insets = new java.awt.Insets(2, 0, 2, 0);
        PanelListMag.add(LinesElesPanel, gridBagConstraints2);
        
        userDefaultsCheckBox.setToolTipText("Check this box to use your defaults for size and location");
        userDefaultsCheckBox.setSelected(false);
        userDefaultsCheckBox.setText("Use my defaults");
        userDefaultsCheckBox.setAlignmentX(0.5F);
        userDefaultsCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                userDefaultsCheckBoxActionPerformed(evt);
            }
        });
        
        gridBagConstraints2 = new java.awt.GridBagConstraints();
        gridBagConstraints2.gridx = 0;
        gridBagConstraints2.gridy = 4;
        gridBagConstraints2.anchor = java.awt.GridBagConstraints.NORTH;
        PanelListMag.add(userDefaultsCheckBox, gridBagConstraints2);
        
        gridBagConstraints1 = new java.awt.GridBagConstraints();
          gridBagConstraints1.gridx = 0;
          gridBagConstraints1.gridy = 1;
          add(PanelListMag, gridBagConstraints1);
          
          PanelBandUnit.setLayout(new java.awt.GridBagLayout());
          java.awt.GridBagConstraints gridBagConstraints3;
          
          PanelBandUnit.setPreferredSize(new java.awt.Dimension(500, 75));
          PanelBandUnit.setMinimumSize(new java.awt.Dimension(300, 75));
          BandPanel.setLayout(new java.awt.BorderLayout());
          
          BandPanel.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
          BandLabel.setText("Channel");
          BandLabel.setToolTipText("Select channel from list");
          BandLabel.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
          BandLabel.setAlignmentX(0.5F);
          BandPanel.add(BandLabel, java.awt.BorderLayout.NORTH);
          
          BandBox.setToolTipText("Select a band/channel number from the list or enter one");
          BandBox.setActionCommand("bandBoxChanged");
          BandBox.addActionListener(new java.awt.event.ActionListener() {
              public void actionPerformed(java.awt.event.ActionEvent evt) {
                  BandBoxActionPerformed(evt);
              }
          });
          
          BandPanel.add(BandBox, java.awt.BorderLayout.WEST);
          
          gridBagConstraints3 = new java.awt.GridBagConstraints();
        gridBagConstraints3.gridx = 0;
        gridBagConstraints3.gridy = 0;
        gridBagConstraints3.gridwidth = java.awt.GridBagConstraints.RELATIVE;
        gridBagConstraints3.ipady = 5;
        gridBagConstraints3.insets = new java.awt.Insets(0, 5, 0, 5);
        PanelBandUnit.add(BandPanel, gridBagConstraints3);
        
        UnitsPanel.setLayout(new java.awt.BorderLayout());
          
          UnitsPanel.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
          UnitLabel.setText("Unit");
          UnitLabel.setToolTipText("Select unit from list after selecting Band");
          UnitLabel.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
          UnitsPanel.add(UnitLabel, java.awt.BorderLayout.NORTH);
          
          UnitBox.setToolTipText("Select a unit from the list or enter a units code");
          UnitBox.setActionCommand("unitBoxChanged");
          UnitBox.addActionListener(new java.awt.event.ActionListener() {
              public void actionPerformed(java.awt.event.ActionEvent evt) {
                  UnitBoxActionPerformed(evt);
              }
          });
          
          UnitsPanel.add(UnitBox, java.awt.BorderLayout.WEST);
          
          gridBagConstraints3 = new java.awt.GridBagConstraints();
        gridBagConstraints3.gridx = 1;
        gridBagConstraints3.gridy = 0;
        gridBagConstraints3.gridwidth = java.awt.GridBagConstraints.RELATIVE;
        gridBagConstraints3.ipadx = 2;
        gridBagConstraints3.ipady = 5;
        gridBagConstraints3.insets = new java.awt.Insets(0, 4, 0, 4);
        PanelBandUnit.add(UnitsPanel, gridBagConstraints3);
        
        gridBagConstraints1 = new java.awt.GridBagConstraints();
        gridBagConstraints1.gridx = 0;
        gridBagConstraints1.gridy = 2;
        add(PanelBandUnit, gridBagConstraints1);
        
        userActionButton.setToolTipText("Click button to "+actionButtonString);
        userActionButton.setText(actionButtonString);
        userActionButton.setActionCommand("user_action");
        userActionButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                userActionButtonActionPerformed(evt);
            }
        });
        
        UserActionPanel.add(userActionButton);
        
        gridBagConstraints1 = new java.awt.GridBagConstraints();
        gridBagConstraints1.gridx = 0;
        gridBagConstraints1.gridy = 5;
        add(UserActionPanel, gridBagConstraints1);
        
        statusLabel.setEditable(false);
        statusLabel.setColumns(40);
        statusLabel.setForeground(java.awt.Color.white);
        statusLabel.setText("First pick an ADDE server");
        statusLabel.setBackground(java.awt.Color.black);
        statusLabel.setPreferredSize(new java.awt.Dimension(440, 20));
        statusLabel.setMinimumSize(new java.awt.Dimension(100, 20));
        statusLabel.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                statusLabelActionPerformed(evt);
            }
        });
        
        PanelStatus.add(statusLabel);
        
        gridBagConstraints1 = new java.awt.GridBagConstraints();
          gridBagConstraints1.gridx = 0;
          gridBagConstraints1.gridy = 7;
          add(PanelStatus, gridBagConstraints1);
          
          PanelLoc.setLayout(new java.awt.BorderLayout());
          
          PanelLoc.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
          jPanel11.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 5, 0));
          
          PlaceLabel.setText("Location:");
          jPanel11.add(PlaceLabel);
          
          LatLonButton.setToolTipText("Click to select lat/lon coordinates");
          LatLonButton.setSelected(true);
          LatLonButton.setLabel("Lat/Lon");
          LatLonButton.setActionCommand("LatLonButt");
          LatLonButton.addChangeListener(new javax.swing.event.ChangeListener() {
              public void stateChanged(javax.swing.event.ChangeEvent evt) {
                  LatLonButtonStateChanged(evt);
              }
          });
          
          jPanel11.add(LatLonButton);
          
          LinEleButton.setToolTipText("Click to select line/element coordinates");
          LinEleButton.setLabel("Line/Ele");
          LinEleButton.setActionCommand("LineEleButt");
          LinEleButton.addChangeListener(new javax.swing.event.ChangeListener() {
              public void stateChanged(javax.swing.event.ChangeEvent evt) {
                  LinEleButtonStateChanged(evt);
              }
          });
          
          jPanel11.add(LinEleButton);
          
          IDButton.setToolTipText("Click to select a station ID (Note: RADAR DATA ONLY!)");
          IDButton.setText("Stn ID");
          IDButton.setActionCommand("IDButt");
          IDButton.addChangeListener(new javax.swing.event.ChangeListener() {
              public void stateChanged(javax.swing.event.ChangeEvent evt) {
                  IDButtonStateChanged(evt);
              }
          });
          
          jPanel11.add(IDButton);
          
          PanelLoc.add(jPanel11, java.awt.BorderLayout.NORTH);
        
        jPanel12.setLayout(new java.awt.GridLayout(2, 2, 5, 0));
          
          LatLineLabel.setText("Latitude");
          LatLineLabel.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
          jPanel12.add(LatLineLabel);
          
          LonEleLabel.setText("Longitude");
          LonEleLabel.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
          jPanel12.add(LonEleLabel);
          
          LatLineText.setToolTipText("Enter latitude or line coordinate or Radar Station ID");
          LatLineText.setText("Lat or line or stn ID value");
          jPanel12.add(LatLineText);
          
          LonEleText.setToolTipText("Enter longitude or element coordinate value");
          LonEleText.setText("Long or Element value");
          jPanel12.add(LonEleText);
          
          PanelLoc.add(jPanel12, java.awt.BorderLayout.SOUTH);
        
        gridBagConstraints1 = new java.awt.GridBagConstraints();
      gridBagConstraints1.gridx = 0;
      gridBagConstraints1.gridy = 3;
      add(PanelLoc, gridBagConstraints1);
      
  }//GEN-END:initComponents

  private void statusLabelActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_statusLabelActionPerformed
      // Add your handling code here:
  }//GEN-LAST:event_statusLabelActionPerformed

  private void userDefaultsCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_userDefaultsCheckBoxActionPerformed
// Add your handling code here:
    useDefaults = userDefaultsCheckBox.isSelected();
  }//GEN-LAST:event_userDefaultsCheckBoxActionPerformed
    
  private void BandBoxActionPerformed (java.awt.event.ActionEvent evt) {//GEN-FIRST:event_BandBoxActionPerformed
    // Add your handling code here:
    int i = BandBox.getSelectedIndex();
    if (i > 0) {
      replaceList(UnitBox,calInfo[1][i-1],"Select Unit");
      int bi = bandListIndex[i-1];
      selectedBand = bandList[i-1];
      selectedBandIndex = i-1;
    
      if (!useDefaults || !gotUserDefaults) {
        double clat = ad[areaIndex][bi].getCenterLatitude();
        double clon = ad[areaIndex][bi].getCenterLongitude();
        if (! (Double.isNaN(clat) || Double.isNaN(clon)) ) {
          setCoordType("E");
          setLocationString(clat+" "+clon);
        }

        resLat = ad[areaIndex][bi].getCenterLatitudeResolution();
        resLon = ad[areaIndex][bi].getCenterLongitudeResolution();

        // if we have resolution data, change the sliders...
        if (! (Double.isNaN(resLat) || Double.isNaN(resLon)) ) {
          doingRes = true;
        }
        setLineMag(1);
        setEleMag(1);

        int nlines= ad[areaIndex][bi].getLines();
        int neles = ad[areaIndex][bi].getElements();
        setNumLinesEles(nlines+" "+neles);
        
        if (useDefaults) gotUserDefaults = true;
      }   
    }
  }//GEN-LAST:event_BandBoxActionPerformed
  
  private void UnitBoxActionPerformed (java.awt.event.ActionEvent evt) {//GEN-FIRST:event_UnitBoxActionPerformed
    // Add your handling code here:
    int unitIndex = UnitBox.getSelectedIndex();
    if (unitIndex > 0 && selectedBandIndex >= 0) 
       selectedUnit = calInfo[0][selectedBandIndex][unitIndex - 1];
    
  }//GEN-LAST:event_UnitBoxActionPerformed
  
  private void DateTimeListValueChanged (javax.swing.event.ListSelectionEvent evt) {//GEN-FIRST:event_DateTimeListValueChanged
    
    selectedDateTime = (String) DateTimeList.getSelectedValue();
    if (selectedDateTime == null) return;
    
    areaIndex = DateTimeList.getSelectedIndex();
    
    double clat = ad[areaIndex][0].getCenterLatitude();
    double clon = ad[areaIndex][0].getCenterLongitude();
    
    if (!useDefaults || !gotUserDefaults) {
      
      if (!(Double.isNaN(clat) || Double.isNaN(clon)) ) {
        setCoordType("E");
        setLocationString(clat+" "+clon);
      }

      resLat = ad[areaIndex][0].getCenterLatitudeResolution();
      resLon = ad[areaIndex][0].getCenterLongitudeResolution();

      // if we have resolution data, change the sliders...
      if (! (Double.isNaN(resLat) || Double.isNaN(resLon)) ) {
        doingRes = true;
      }
      setLineMag(1);
      setEleMag(1);

      int nlines= ad[areaIndex][0].getLines();
      int neles = ad[areaIndex][0].getElements();
      setNumLinesEles(nlines+" "+neles);

      if (useDefaults) gotUserDefaults = true;
    }
    
    ArrayList arrayBandList = new ArrayList();
    ArrayList arrayBandIndex = new ArrayList();
    ArrayList arrayCalInfo = new ArrayList();
    asb = new AddeSatBands(asi.getBandNames());
    String[][] ci;
    String[] bandInfo = null;
    
    for (int na=0; na<ad[areaIndex].length; na++) {
      
      int nbands = ad[areaIndex][na].getNumberOfBands();
      int [] bl = ad[areaIndex][na].getBands();
      String [] sb = new String[nbands];
      Vector[] vb = ad[areaIndex][na].getCalInfo();
      int sid = ad[areaIndex][na].getSensorID();
      String srcType = ad[areaIndex][na].getSourceType() ;
      bandInfo = asb.getBandDescr(sid, srcType);
          
      for (int k=0; k<nbands; k++) {
        
        sb[k] = Integer.toString(bl[k]);
        arrayBandList.add(sb[k]);
        arrayBandIndex.add(new Integer(na));
        ci = new String[2][1];
        ci[0][0] = "RAW";
        ci[1][0] = "Raw values";
        if (vb[k] != null) {
          int vbnum = vb[k].size()/2;
          if (vbnum > 0) {
            ci = new String[2][vbnum];
            for (int j=0; j<vbnum; j++) {
              ci[0][j] = (String) vb[k].elementAt(2*j);
              ci[1][j] = (String) vb[k].elementAt(2*j + 1);
            }
          }
        }
        arrayCalInfo.add(ci);
      }
    }

    int ns = arrayBandList.size();
    if (ns != 0) {
      
      bandList = new String[ns];
      bandListIndex = new int[ns];
      String[] bandListNames = new String[ns];
      calInfo = new String[2][ns][];
      for (int ks=0; ks<ns; ks++) {
        bandList[ks] = (String) arrayBandList.get(ks);
        bandListNames[ks] = bandList[ks];
        if (bandInfo != null) {
          int bn = (Integer.valueOf(bandList[ks])).intValue();
          if (bn >0 && bn < bandInfo.length) {
            bandListNames[ks] = bandList[ks]+" - "+bandInfo[bn];
          }
        }
        
        bandListIndex[ks] = ( (Integer) arrayBandIndex.get(ks)).intValue();
        ci = (String[][]) arrayCalInfo.get(ks);
        int nc = ci[0].length;
        calInfo[0][ks] = ci[0];
        calInfo[1][ks] = ci[1];
      }

      if (bandList.length == 1) {
        replaceList(BandBox,bandListNames,null);
        replaceList(UnitBox,calInfo[1][0],"Select Unit");
        selectedBand = bandList[0];
        selectedBandIndex = 0;
      } else {
        replaceList(BandBox, bandListNames, "Select band");
        replaceList(UnitBox, null, " ");
        selectedBand = bandList[0];
        selectedBandIndex = 1;
      }
    }
    status("Set the other parameters you want");
  }//GEN-LAST:event_DateTimeListValueChanged
  
  public synchronized void status(String m) {
    /*
    String mt = m.trim();
    int k = (mt.length() > 80) ? 80 : mt.length();
    statusLabel.setText(mt.substring(0,k));
    System.out.println("Status:"+mt.substring(0,k));
     */
    statusLabel.setText(m);
    //System.out.println("Status:"+m);
    statusLabel.validate();
  }
  
  public void addActionListener(ActionListener o) {
    al = o;
  }

  public void removeActionListener(ActionListener o) {
    if (o == al) al = null;
  }

  private void userActionButtonActionPerformed (java.awt.event.ActionEvent evt) {//GEN-FIRST:event_userActionButtonActionPerformed
    // Add your handling code here:
    status("Processing...please wait...");

    String prop = getServer()+"|"+getGroup()+"|"+getDescr();
    dataProp.put(prop+"|CoordType",getCoordType());
    dataProp.put(prop+"|Location",getLocationString());
    dataProp.put(prop+"|Size",getNumLinesEles());
    dataProp.put(prop+"|Mag",getMag());
    if (getUserName() != null) 
           dataProp.put(getServer()+"|UserName",getUserName());
    if (getProjectNumber() != null) 
           dataProp.put(getServer()+"|ProjectNumber",getProjectNumber());
    if (serverList != null) dataProp.put("user|server|list",serverList.toString());
    
    try {
      FileOutputStream fo = new FileOutputStream(propFile);
      dataProp.save(fo,"GetAreaGUI");
      fo.close();
    } catch (Exception es) {;}
   
    String loc = " ";
    if (coordType.equals("E")) {
      loc = "&latlon=";
    } else if (coordType.equals("I")) {
      loc = "&linele=";
    } else if (coordType.equals("S")) {
      loc = "&id=";
    }
      
    String up = getUserName();
    if (up == null || up.length() < 2) {
      up = " ";
    } else {
      up = "&user="+getUserName()+"&proj="+getProjectNumber();
    }
    
    String un = getUnit();
    if (un == null || un.length()<3) un="BRIT";
    imageList = new ArrayList();
    
    if (multipleImages) {
      int numimages = DateTimeList.getSelectedIndices().length;
      StringBuffer cmdbuf = new StringBuffer();

      for (int i=0; i<numimages; i++) {

        String addecmd = new String
        ("adde://"+getServer()+"/imagedata?group="+getGroup()+
          "&descr="+getDescr()+loc+getLocationString()+
          "&size="+getNumLinesEles()+"&day="+getDay(i)+"&time="+getTime(i)+" "+
          getTime(i)+" I "+
          "&band="+getBand()+"&unit="+un+"&mag="+getMag()+ up + "&version=1");
        cmdbuf.append(addecmd);
        imageList.add(addecmd);
        if (i+1 < numimages) cmdbuf.append("|");
      }
      cmdout = cmdbuf.toString();

    } else {
      cmdout = "adde://"+getServer()+"/imagedata?group="+getGroup()+
         "&descr="+getDescr()+loc+getLocationString()+
         "&size="+getNumLinesEles()+"&day="+getDay()+"&time="+getTime()+" "+
         getTime()+" I "+
         "&band="+getBand()+"&unit="+un+"&mag="+getMag()+ up +
         "&version=1";
      imageList.add(cmdout);
    }
    System.out.println("cmdout = "+cmdout);

    if (al != null) {
      al.actionPerformed(
            new ActionEvent(this,ActionEvent.ACTION_PERFORMED, cmdout) );
    }

    if (closeOnAction && (dialog != null)) dialog.dispose();
    
  }//GEN-LAST:event_userActionButtonActionPerformed
  
  public List getImageList() {
    return (List) imageList;
  }

  public String toString() {
    return cmdout;
  }

  private synchronized void setLocButtonLabel() {
    if (LatLonButton.isSelected()) {
      LatLineLabel.setText("Latitude");
      LonEleLabel.setText("Longitude");
      coordType = "E";
      
    } else if (LinEleButton.isSelected()) {
      LatLineLabel.setText("Image Line");
      LonEleLabel.setText("Image Element");
      coordType = "I";
      
    } else if (IDButton.isSelected()) {
      LatLineLabel.setText("Radar Station ID");
      LonEleLabel.setText(" ");
      coordType = "S";
    }
  }
  private void IDButtonStateChanged (javax.swing.event.ChangeEvent evt) {//GEN-FIRST:event_IDButtonStateChanged
    // Add your handling code here:
    setLocButtonLabel();
  }//GEN-LAST:event_IDButtonStateChanged
  
  private void LinEleButtonStateChanged (javax.swing.event.ChangeEvent evt) {//GEN-FIRST:event_LinEleButtonStateChanged
    // Add your handling code here:
    setLocButtonLabel();
  }//GEN-LAST:event_LinEleButtonStateChanged
  
  private void LatLonButtonStateChanged (javax.swing.event.ChangeEvent evt) {//GEN-FIRST:event_LatLonButtonStateChanged
    // Add your handling code here:
    setLocButtonLabel();
    
  }//GEN-LAST:event_LatLonButtonStateChanged
  
  private void EMagSliderStateChanged (javax.swing.event.ChangeEvent evt) {//GEN-FIRST:event_EMagSliderStateChanged
    // Add your handling code here:
    int v = EMagSlider.getValue();
    setEleMag(v - 50);
    if (baseNumEles < 1 ) return;
    int x = baseNumEles * EMagValue;
    if (EMagValue < 0) x = baseNumEles / -EMagValue;
    NumElesText.setText(Integer.toString(x));
  }//GEN-LAST:event_EMagSliderStateChanged
  
  private void LMagSliderStateChanged (javax.swing.event.ChangeEvent evt) {//GEN-FIRST:event_LMagSliderStateChanged
    // Add your handling code here:
    int v = LMagSlider.getValue();
    setLineMag(v - 50);
    setEleMag(v - 50);
    if (baseNumLines < 1 ) return;
    int x = baseNumLines * LMagValue;
    if (LMagValue < 0) x = baseNumLines / -LMagValue;
    NumLinesText.setText(Integer.toString(x));
    
    
  }//GEN-LAST:event_LMagSliderStateChanged
  
  private void DescrSelectorActionPerformed (java.awt.event.ActionEvent evt) {//GEN-FIRST:event_DescrSelectorActionPerformed
    // Add your handling code here:
    if (serverUpdated) return;
    if (groupUpdated) return;
    areaIndex = -1;
    Thread tdsap = new Thread() {
      public void run() {
        int i = DescrSelector.getSelectedIndex();
        String[] dl = asi.getDatasetList();
        if ( (i > 0) && (dl != null) ) {
          selectedDescr = dl[i-1];  // there was a header in the combo box
          status("Getting dates and times from server...may take a while!!");
          asi.setSelectedDataset( selectedDescr );
          String[] times = asi.getDateTimeList();
          ad = asi.getAreaDirectories();
          DateTimeList.setListData(times);
          DateTimeList.revalidate();
          status("Now pick a day & time from the list, or set a Position number");
          String prop = getServer()+"|"+getGroup()+"|"+getDescr();
          String pp = (String) dataProp.get(prop+"|CoordType");
          gotUserDefaults = false;
          if (pp != null) {setCoordType(pp); gotUserDefaults = true;}
          pp = (String) dataProp.get(prop+"|Location");
          if (pp != null) {setLocationString(pp); gotUserDefaults = true;}
          pp = (String) dataProp.get(prop+"|Size");
          if (pp != null) {setNumLinesEles(pp); gotUserDefaults = true;}
          pp = (String) dataProp.get(prop+"|Mag");
          if (pp != null) {setMag(pp); gotUserDefaults = true;}
        }
      }
    };
    
    tdsap.start();
    doingRes = false;
    setLineMag(1);
    setEleMag(1);
    setCoordType("E");
    setLocationString(null);
    setNumLinesEles(null);
    
    String[] ld = {"Day and times for","selected dataset will","appear here"};
    DateTimeList.setListData(ld);
    DateTimeList.revalidate();
    
  }//GEN-LAST:event_DescrSelectorActionPerformed
  
  private void GroupSelectorActionPerformed (java.awt.event.ActionEvent evt) {//GEN-FIRST:event_GroupSelectorActionPerformed
    // Add your handling code here:
    if (serverUpdated) return;
    areaIndex = -1;
    Thread tgsap = new Thread() {
      public void run() {
        selectedGroup = (String) GroupSelector.getSelectedItem();
        asi.setSelectedGroup( selectedGroup );
        bandNames = asi.getBandNames();
        String[] dl = asi.getDatasetListDescriptions();
        groupUpdated = true;
        replaceList(DescrSelector, dl, "Select Dataset");
        groupUpdated = false;
        status("Now choose a Dataset from the list");
        replaceList(UnitBox, null, "        ");
        replaceList(BandBox, null, "        ");
      }
    };
    tgsap.start();
    DateTimeList.removeAll();
    String[] ld = {"Day and times for","selected dataset will","appear here"};
    DateTimeList.setListData(ld);
    DateTimeList.revalidate();
    
  }//GEN-LAST:event_GroupSelectorActionPerformed
  
  private void ServerSelectorActionPerformed (java.awt.event.ActionEvent evt) {//GEN-FIRST:event_ServerSelectorActionPerformed
    
    Thread tssap = new Thread() {
      public void run() {
        serverUpdated = true;
        replaceList(GroupSelector, null, "Please wait...");
        status("Reading information from server...");
        selectedServer = (String) ServerSelector.getSelectedItem();
        int sstat = -1;
        while (sstat == -1) {
          // first see if a user name & proj number have been set...
          String un = getUserName();
          if (un == null || un.length() < 2) {
            un = (String) dataProp.get(selectedServer+"|UserName");
            String pn = (String) dataProp.get(selectedServer+"|ProjectNumber");
            if (un != null) {
              setUserName(un);
              setProjectNumber(pn);
              String ups = "user="+un+"&proj="+pn;
              asi.setUserIDandProjString(ups);
            }
          } else {
            String ups = "user="+un+"&proj="+getProjectNumber();
            asi.setUserIDandProjString(ups);
          }
          // now see if the Server can be found and read from...
          sstat = asi.setSelectedServer(selectedServer, "image");
          if (sstat == -1) {
            String pus = JOptionPane.showInputDialog(
            "User ID and project number required by this server!\nPlease enter them here (eg., jack 1234)");
            if (pus != null) {
              StringTokenizer stp = new StringTokenizer(pus," ");
              if (stp.countTokens() != 2) continue;
              setUserName(stp.nextToken());
              setProjectNumber(stp.nextToken());
            } else {
              sstat = -5;
            }
          }
        }
        
        if (sstat >= 0) {
          if (ServerSelector.getSelectedIndex() == -1) {
            // append server name onto list
            ServerSelector.addItem(selectedServer);
            // and onto properties list
            serverList.append(","+selectedServer);
          }
          String[] gl = asi.getGroupList();
          String prompt;
          if (gl == null) {
            GroupSelector.setEditable(true);
            prompt = "Enter the group name";
          } else {
            GroupSelector.setEditable(false);
            prompt = "Select group";
          }
          replaceList(GroupSelector, gl, prompt);
          serverUpdated = false;
          status("Now - "+prompt+" to use...");
        } else {
          status("Pick a different server!");
          serverUpdated = false;
        }
      }
    };
    tssap.start();
    areaIndex = -1;
    DescrSelector.removeAllItems();
    DateTimeList.removeAll();
    
    String[] ld = {"Day and times for","selected dataset will","appear here"};
    DateTimeList.setListData(ld);
    DateTimeList.revalidate();
    DescrSelector.revalidate();
    // Add your handling code here:
  }//GEN-LAST:event_ServerSelectorActionPerformed
  
  private synchronized void replaceList(JComboBox b, String[] s, String first) {
    if (b.getItemCount() > 0) b.removeAllItems();
    if (first != null) b.addItem(first);
    if (s != null) {
      for (int i=0; i < s.length; i++) {
        b.addItem(s[i].trim());
      }
    }
    b.setSelectedIndex(0);
    b.revalidate();
  }
    
    /**
     * @param args the command line arguments
     */
    public static void main (String args[]) {
      GetAreaGUI gag = new GetAreaGUI ("avoid pain", false, false);
      gag.setUserName("tomw");
      gag.setProjectNumber("1234");
      gag.show();
      
    }
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel PanelSGD;
    private javax.swing.JPanel PanelSG;
    private javax.swing.JPanel PanelServer;
    private javax.swing.JLabel ServerLabel;
    private javax.swing.JComboBox ServerSelector;
    private javax.swing.JPanel PanelGroup;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JComboBox GroupSelector;
    private javax.swing.JPanel PanelDescr;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JComboBox DescrSelector;
    private javax.swing.JPanel PanelListMag;
    private javax.swing.JPanel PanelMag;
    private javax.swing.JPanel jPanel13;
    private javax.swing.JPanel jPanel9;
    private javax.swing.JSlider LMagSlider;
    private javax.swing.JLabel LMagLabel;
    private javax.swing.JPanel jPanel10;
    private javax.swing.JLabel EMagLabel;
    private javax.swing.JSlider EMagSlider;
    private javax.swing.JLabel DateTimeLabel;
    private javax.swing.JPanel PanelList;
    private javax.swing.JScrollPane DateTimeScrollPanel;
    private javax.swing.JList DateTimeList;
    private javax.swing.JPanel LinesElesPanel;
    private javax.swing.JLabel SizeLabel;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JLabel NumLinesLabel;
    private javax.swing.JTextField NumLinesText;
    private javax.swing.JLabel NumElesLabel;
    private javax.swing.JTextField NumElesText;
    private javax.swing.JCheckBox userDefaultsCheckBox;
    private javax.swing.JPanel PanelBandUnit;
    private javax.swing.JPanel BandPanel;
    private javax.swing.JLabel BandLabel;
    private javax.swing.JComboBox BandBox;
    private javax.swing.JPanel UnitsPanel;
    private javax.swing.JLabel UnitLabel;
    private javax.swing.JComboBox UnitBox;
    private javax.swing.JPanel UserActionPanel;
    private javax.swing.JButton userActionButton;
    private javax.swing.JPanel PanelStatus;
    private javax.swing.JTextField statusLabel;
    private javax.swing.JPanel PanelLoc;
    private javax.swing.JPanel jPanel11;
    private javax.swing.JLabel PlaceLabel;
    private javax.swing.JRadioButton LatLonButton;
    private javax.swing.JRadioButton LinEleButton;
    private javax.swing.JRadioButton IDButton;
    private javax.swing.JPanel jPanel12;
    private javax.swing.JLabel LatLineLabel;
    private javax.swing.JLabel LonEleLabel;
    private javax.swing.JTextField LatLineText;
    private javax.swing.JTextField LonEleText;
    // End of variables declaration//GEN-END:variables
    private javax.swing.ButtonGroup buttGroupLoc;
    private int LMagValue, EMagValue;
    private String calText = null;
}
