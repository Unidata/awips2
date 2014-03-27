<?xml version="1.0" encoding="ISO-8859-1"?>
<!-- 
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
  * 
 * Coverage.xsl
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 ******************************************************************
 * Mar 02, 2014 2789      dhladky      XSLT transformation of other versions to current.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
-->



<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="1.0">
    <xsl:import
        href="./com.raytheon.uf.common.datadelivery.registry.GriddedCoverage.xsl" />
    <xsl:output method="xml" encoding="utf-8" indent="yes" />
    <xsl:template match="coverage" name="coverage">

        <coverage xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
            <xsl:if test="@xsi:type != ''">
                <xsl:attribute name="xsi:type">
    			<xsl:value-of select="@xsi:type" />
			</xsl:attribute>
            </xsl:if>

            <!-- figure out which type of coverage we are processing -->
            <xsl:if test="@xsi:type='griddedCoverage'">
                <xsl:call-template name="griddedCoverage" />
            </xsl:if>

            <!-- envelope templates -->
            <xsl:if test="envelope">
                <xsl:apply-templates select="envelope" />
            </xsl:if>
            <xsl:if test="requestEnvelope">
                <xsl:apply-templates select="requestEnvelope" />
            </xsl:if>

        </coverage>
    </xsl:template>

    <xsl:template match="requestEnvelope">
        <xsl:element name="requestEnvelope">
            <xsl:if test="@maxY != ''">
                <xsl:attribute name="maxY">
        		<xsl:value-of select="@maxY" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@maxY != ''">
                <xsl:attribute name="minY">
        		<xsl:value-of select="@minY" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@maxX != ''">
                <xsl:attribute name="maxX">
       		 	<xsl:value-of select="@maxX" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@minX != ''">
                <xsl:attribute name="minX">
        		<xsl:value-of select="@minX" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="crs">
                <xsl:element name="crs">
                    <xsl:value-of select="crs" />
                </xsl:element>
            </xsl:if>
        </xsl:element>
    </xsl:template>

    <xsl:template match="envelope">
        <xsl:element name="envelope">
            <xsl:if test="@maxY != ''">
                <xsl:attribute name="maxY">
        		<xsl:value-of select="@maxY" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@maxY != ''">
                <xsl:attribute name="minY">
        		<xsl:value-of select="@minY" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@maxX != ''">
                <xsl:attribute name="maxX">
       		 	<xsl:value-of select="@maxX" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@minX != ''">
                <xsl:attribute name="minX">
        		<xsl:value-of select="@minX" />
			</xsl:attribute>
            </xsl:if>
            <xsl:if test="crs">
                <xsl:element name="crs">
                    <xsl:value-of select="crs" />
                </xsl:element>
            </xsl:if>
        </xsl:element>
    </xsl:template>

</xsl:stylesheet>