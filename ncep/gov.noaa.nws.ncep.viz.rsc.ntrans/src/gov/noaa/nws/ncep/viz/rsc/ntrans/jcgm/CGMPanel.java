/*
 * <copyright> Copyright 1997-2003 BBNT Solutions, LLC under sponsorship of the
 * Defense Advanced Research Projects Agency (DARPA).
 * Copyright 2009 Swiss AviationSoftware Ltd.
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the Cougaar Open Source License as published by DARPA on
 * the Cougaar Open Source Website (www.cougaar.org).
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm;

import java.awt.Dimension;
import java.awt.Graphics;
import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.zip.GZIPInputStream;

import javax.swing.JPanel;


/**
 * A panel to display a CGM graphic.
 * 
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class CGMPanel extends JPanel {
	private static final long serialVersionUID = -6773054268386447080L;
	
	private CGMDisplay cgmDisplay;
    private int width = 0, height = 0;
    private double dpi = 96;
    private final double dpiStep = 75;
    
    public CGMPanel() {
    	// do nothing
    }
    
    public CGMPanel(CGMDisplay d) {
        this.cgmDisplay = d;
    }
    
    public void open(File cgmFile) throws IOException {
    	if (cgmFile == null)
    		throw new NullPointerException("unexpected null parameter");
    	
    	InputStream inputStream;
    	String cgmFileName = cgmFile.getName();
		if (cgmFileName.endsWith(".cgm.gz") || cgmFileName.endsWith(".cgmz")) {
    		inputStream = new GZIPInputStream(new FileInputStream(cgmFile));
    	}
    	else {
    		inputStream = new FileInputStream(cgmFile);
    	}
		DataInputStream in = new DataInputStream(new BufferedInputStream(inputStream));
		CGM cgm = new CGM();
		cgm.read(in);
		in.close();
		this.cgmDisplay = new CGMDisplay(cgm);
    }
    
	public void open(URL cgmURL) throws IOException {
		if (cgmURL == null) {
			throw new NullPointerException("null cgm url");
		}

		URLConnection c = cgmURL.openConnection();

		DataInputStream in = new DataInputStream(new BufferedInputStream(
				c.getInputStream()));

		CGM cgm = new CGM();
		cgm.read(in);
		in.close();
		this.cgmDisplay = new CGMDisplay(cgm);
	}

    @Override
    public Dimension getPreferredSize() {
    	if (this.cgmDisplay == null)
    		return new Dimension(200, 200);
    	
    	return this.cgmDisplay.getCGM().getSize(this.dpi);
	}
    
    public void zoomIn() {
    	this.dpi += this.dpiStep;
    }

	public void zoomOut() {
    	this.dpi -= this.dpiStep;
	}
	
	@Override
	public void paint(Graphics g) {
		if (this.cgmDisplay == null)
			return;
		
        this.cgmDisplay.reset();
        int W0 = getSize().width, H0 = getSize().height;
        if (W0 != this.width || H0 != this.height || !this.cgmDisplay.isScaled()) {
            this.width = W0;
            this.height = H0;
            this.cgmDisplay.scale(g, this.width, this.height);
        }
        this.cgmDisplay.paint(g);
    }

}

/*
 * vim:encoding=utf8
 */
