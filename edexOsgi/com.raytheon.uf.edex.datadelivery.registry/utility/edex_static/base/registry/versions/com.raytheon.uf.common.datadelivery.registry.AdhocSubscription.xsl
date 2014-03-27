<?xml version="1.0" encoding="ISO-8859-1"?>
<!-- * This software was developed and / or modified by Raytheon Company, 
    * pursuant to Contract DG133W-05-CQ-1067 with the US Government. * * U.S. 
    EXPORT CONTROLLED TECHNICAL DATA * This software product contains export-restricted 
    data whose * export/transfer/disclosure is restricted by U.S. law. Dissemination 
    * to non-U.S. persons whether in the United States or abroad requires * an 
    export license or other authorization. * * Contractor Name: Raytheon Company 
    * Contractor Address: 6825 Pine Street, Suite 340 * Mail Stop B8 * Omaha, 
    NE 68106 * 402.291.0100 * * See the AWIPS II Master Rights File ("Master 
    Rights File.pdf") for * further licensing information. * * Adhocsubscription.xsl 
    * * <pre> * * SOFTWARE HISTORY * * Date Ticket# Engineer Description ****************************************************************** 
    * Mar 02, 2014 2789 dhladky XSLT transformation of other versions to current. 
    * * </pre> * * @author dhladky * @version 1.0 -->


<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="1.0">
    <xsl:import
        href="./com.raytheon.uf.common.datadelivery.registry.Subscription.xsl" />
    <xsl:output method="xml" encoding="utf-8" indent="yes" />
    <xsl:template match="adhocSubscription">

        <adhocSubscription xmlns:ns2="urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0"
            xmlns:ns4="http://www.w3.org/2005/08/addressing" xmlns:ns3="http://www.w3.org/1999/xlink"
            xmlns:ns9="com.raytheon.uf.common.datadelivery.registry"
            xmlns:ns5="urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0"
            xmlns:ns6="urn:oasis:names:tc:ebxml-regrep:xsd:query:4.0"
            xmlns:ns7="urn:oasis:names:tc:ebxml-regrep:xsd:spi:4.0"
            xmlns:ns8="urn:oasis:names:tc:ebxml-regrep:xsd:lcm:4.0">

            <xsl:call-template name="subscription" />

        </adhocSubscription>

    </xsl:template>

</xsl:stylesheet>

