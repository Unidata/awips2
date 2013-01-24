/////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1998, California Institute of Technology.  
// ALL RIGHTS RESERVED.   U.S. Government Sponsorship acknowledged. 
//
// Please read the full copyright notice in the file COPYRIGHT
// in this directory.
//
// Author: Jake Hamby, NASA/Jet Propulsion Laboratory
//         Jake.Hamby@jpl.nasa.gov
/////////////////////////////////////////////////////////////////////////////

package dods.util.geturl.gui;

import java.awt.Button;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Label;
import java.awt.TextArea;
import java.awt.TextField;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.CharArrayWriter;
import java.io.PrintWriter;

import dods.dap.DAS;
import dods.dap.DConnect;
import dods.dap.DDS;
import dods.dap.DataDDS;

/** Geturl frame. */
public class GeturlFrame extends Frame {
    /** True if we are an applet. */
    protected boolean isApplet;

    /** The TextField containing the URL to retrieve. */
    protected TextField urlField;

    /** The TextArea where output should be written. */
    protected TextArea outputArea;

    /** The "Get DAS", "Get DDS", and "Get Data" buttons. */
    protected Button getDASButton, getDDSButton, getDataButton;

    public GeturlFrame(boolean isApplet) {
        super("DODS Geturl Applet");
        setSize(640, 480);
        this.isApplet = isApplet;

        // add URL TextField, buttons, and output TextArea
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        setLayout(gbl);

        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridwidth = 1;
        gbc.gridy = 0;
        Label urlLabel = new Label("URL:");
        gbl.setConstraints(urlLabel, gbc);
        add(urlLabel);

        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridwidth = 4;
        urlField = new TextField(70);
        gbl.setConstraints(urlField, gbc);
        add(urlField);

        gbc.gridwidth = 1;
        gbc.gridy = 1;
        getDASButton = new Button("Get DAS");
        getDASButton.addActionListener(new GetDASListener());
        gbl.setConstraints(getDASButton, gbc);
        add(getDASButton);

        getDDSButton = new Button("Get DDS");
        getDDSButton.addActionListener(new GetDDSListener());
        gbl.setConstraints(getDDSButton, gbc);
        add(getDDSButton);

        getDataButton = new Button("Get Data");
        getDataButton.addActionListener(new GetDataListener());
        gbl.setConstraints(getDataButton, gbc);
        add(getDataButton);

        gbc.gridwidth = 5;
        gbc.gridy = 2;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        gbc.insets = new Insets(0, 5, 0, 5); // 5 pixel insets on left and right
        outputArea = new TextArea(100, 200);
        outputArea.setEditable(false);
        gbl.setConstraints(outputArea, gbc);
        add(outputArea);

        // set listener for window close
        addWindowListener(new WindowClosedListener(this));

        // finally, show window
        show();
    }

    /**
     * A helper method to enable or disable the other buttons while the user is
     * downloading, so they don't click multiple times.
     * 
     * @param b
     *            if true, enable the buttons; else, disable them.
     */
    private void setButtonsEnabled(boolean b) {
        getDASButton.setEnabled(b);
        getDDSButton.setEnabled(b);
        getDataButton.setEnabled(b);
    }

    private class GetDASListener implements ActionListener, Runnable {
        public void actionPerformed(ActionEvent evt) {
            try {
                // deactivate the other buttons so the user doesn't click them
                // now
                setButtonsEnabled(false);
                // run DConnect in a separate thread from AWT event thread
                Thread runThread = new Thread(this);
                runThread.start();
            } catch (IllegalThreadStateException e) {
                outputArea.setText(e.toString());
                setButtonsEnabled(true);
            }
        }

        public void run() {
            try {
                DConnect url = new DConnect(urlField.getText());
                DAS das = url.getDAS();
                CharArrayWriter aw = new CharArrayWriter();
                das.print(new PrintWriter(aw));
                outputArea.setText(aw.toString());
                // reactivate the other buttons
                setButtonsEnabled(true);
            } catch (Exception e) {
                outputArea.setText(e.toString());
                setButtonsEnabled(true);
            }
        }
    }

    private class GetDDSListener implements ActionListener, Runnable {
        public void actionPerformed(ActionEvent evt) {
            try {
                // deactivate the other buttons so the user doesn't click them
                // now
                setButtonsEnabled(false);
                // run DConnect in a separate thread from AWT event thread
                Thread runThread = new Thread(this);
                runThread.start();
            } catch (IllegalThreadStateException e) {
                outputArea.setText(e.toString());
                setButtonsEnabled(true);
            }
        }

        public void run() {
            try {
                DConnect url = new DConnect(urlField.getText());
                DDS dds = url.getDDS();
                CharArrayWriter aw = new CharArrayWriter();
                dds.print(new PrintWriter(aw));
                outputArea.setText(aw.toString());
                // reactivate the other buttons
                setButtonsEnabled(true);
            } catch (Exception e) {
                outputArea.setText(e.toString());
                setButtonsEnabled(true);
            }
        }
    }

    private class GetDataListener implements ActionListener, Runnable {
        public void actionPerformed(ActionEvent evt) {
            try {
                // deactivate the other buttons so the user doesn't click them
                // now
                setButtonsEnabled(false);
                // run DConnect in a separate thread from AWT event thread
                // NOTE: StatusWindow cancel button won't work otherwise!
                Thread runThread = new Thread(this);
                runThread.start();
            } catch (IllegalThreadStateException e) {
                outputArea.setText(e.toString());
                setButtonsEnabled(true);
            }
        }

        public void run() {
            try {
                DConnect url = new DConnect(urlField.getText());
                DataDDS dds = url.getData(new StatusWindow(urlField.getText()));
                CharArrayWriter aw = new CharArrayWriter();
                dds.printVal(new PrintWriter(aw));
                outputArea.setText(aw.toString());
                // reactivate the other buttons
                setButtonsEnabled(true);
            } catch (Exception e) {
                outputArea.setText(e.toString());
                setButtonsEnabled(true);
            }
        }
    }

    private class WindowClosedListener extends WindowAdapter {
        GeturlFrame myFrame;

        WindowClosedListener(GeturlFrame myFrame) {
            this.myFrame = myFrame;
        }

        public void windowClosing(WindowEvent evt) {
            myFrame.dispose();
            if (myFrame.isApplet == false)
                System.exit(0);
        }
    }
}
