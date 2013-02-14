/*
* The following software products were developed by Raytheon:
*
* ADE (AWIPS Development Environment) software
* CAVE (Common AWIPS Visualization Environment) software
* EDEX (Environmental Data Exchange) software
* uFrame™ (Universal Framework) software
*
* Copyright (c) 2010 Raytheon Co.
* All rights reserved. This program and the accompanying materials
* are made available under the terms of the Eclipse Public License v1.0
* which accompanies this distribution, and is available at
* http://www.eclipse.org/org/documents/epl-v10.php
*
*
* Contractor Name: Raytheon Company
* Contractor Address:
* 6825 Pine Street, Suite 340
* Mail Stop B8
* Omaha, NE 68106
* 402.291.0100
*
*
* SOFTWARE HISTORY
*
* Date         Ticket#    Engineer    Description
* ------------ ---------- ----------- --------------------------
* Feb 28, 2012            bclement     Initial creation
*
*/ 
package com.raytheon.uf.edex.ogc.common.output;

import java.awt.image.RenderedImage;
import java.io.PrintWriter;
import java.nio.ByteBuffer;
import java.util.Iterator;

import javax.imageio.ImageIO;
import javax.imageio.ImageWriter;
import javax.imageio.stream.ImageOutputStream;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;

import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;
import com.raytheon.uf.edex.ogc.common.OgcResponse;

/**
 *
 * @author bclement
 * @version 1.0	
 */
public class OgcResponseOutput {

	public static void output(OgcResponse response, HttpServletResponse httpRes)
			throws Exception {
		switch (response.getType()) {
		case TEXT:
			sendText(response, httpRes);
			break;
		case IMAGE:
			sendImage(response, httpRes);
			break;
		case BYTE:
		case MULTIPART:
			sendByteArray(response, httpRes);
			break;
		default:
			throw new Exception("Unsupported type: " + response.getType());
		}
	}


	protected static void checkError(OgcResponse response,
			HttpServletResponse httpRes) {
		int code = HttpServletResponse.SC_OK;
		switch (response.getError()) {
		case INT_ERR:
			code = HttpServletResponse.SC_INTERNAL_SERVER_ERROR;
			break;
		case BAD_REQ:
			code = HttpServletResponse.SC_BAD_REQUEST;
			break;
		case NOT_IMPLEMENTED:
			code = HttpServletResponse.SC_NOT_IMPLEMENTED;
			break;
		}
		httpRes.setStatus(code);
	}

	public static void sendText(OgcResponse response,
			HttpServletResponse httpRes) throws Exception {
		httpRes.setContentType(response.getMimetype());
		checkError(response, httpRes);
		Object obj = response.getBody();
		ServletOutputStream out = null;
		PrintWriter writer = null;
		try {
			if (obj instanceof byte[]) {
				// UTF8 bytes
				httpRes.setCharacterEncoding("UTF-8");
				out = httpRes.getOutputStream();
				out.write((byte[]) obj);
			} else {
				writer = httpRes.getWriter();
				writer.write(obj.toString());
			}
		} catch (Exception e) {
			httpRes.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			throw e;
		} finally {
			if (out != null) {
				out.close();
			}
			if (writer != null) {
				writer.close();
			}
		}
	}

	public static void sendImage(OgcResponse response,
			HttpServletResponse httpRes) throws Exception {
		String mimetype = response.getMimetype();
		checkError(response, httpRes);
		httpRes.setContentType(mimetype);
		ImageOutputStream out = null;
		try {
			Iterator<?> it = ImageIO.getImageWritersByMIMEType(mimetype);
			if (!it.hasNext()) {
				throw new OgcException(Code.InvalidFormat,
						"Format not supported: " + mimetype);
			}
			ImageWriter writer = (ImageWriter) it.next();
			out = ImageIO.createImageOutputStream(httpRes.getOutputStream());
			writer.setOutput(out);
			writer.write((RenderedImage) response.getBody());
		} catch (Exception e) {
			httpRes.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			throw e;
		} finally {

			if (out != null) {
				out.close();
			}
		}
	}

	public static void sendByteArray(OgcResponse response,
			HttpServletResponse httpRes) throws Exception {
		httpRes.setContentType(response.getMimetype());
		checkError(response, httpRes);
		Object obj = response.getBody();
		byte[] arr;
		if (obj instanceof byte[]) {
			arr = (byte[]) obj;
		} else if (obj instanceof ByteBuffer) {
			arr = ((ByteBuffer) obj).array();
		} else {
			throw new Exception("Unsupported class: " + obj.getClass());
		}
		ServletOutputStream out = null;
		try {
			out = httpRes.getOutputStream();
			out.write(arr);
		} catch (Exception e) {
			httpRes.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			throw e;
		} finally {
			if (out != null) {
				out.close();
			}
		}
	}
}
