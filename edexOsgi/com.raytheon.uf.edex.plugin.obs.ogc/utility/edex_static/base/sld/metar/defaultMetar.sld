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
 -->

<?xml version="1.0" encoding="ISO-8859-1"?>
<StyledLayerDescriptor version="1.0.0"
 xsi:schemaLocation="http://www.opengis.net/sld StyledLayerDescriptor.xsd"
 xmlns="http://www.opengis.net/sld"
 xmlns:ogc="http://www.opengis.net/ogc"
 xmlns:xlink="http://www.w3.org/1999/xlink"
 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <!-- a Named Layer is the basic building block of an SLD document -->
  <NamedLayer>
    <Name>metar</Name>
    <UserStyle>
    <!-- Styles can have names, titles and abstracts -->
      <Title>default</Title>
      <Name>default</Name>
      <Abstract>A sample style that draws a point based on visibility/ceiling distance</Abstract>
      <!-- FeatureTypeStyles describe how to render different features -->
      <!-- A FeatureTypeStyle for rendering points -->
      <FeatureTypeStyle>
         <Rule>
           <Name>rule5</Name>
          <Title>Vis &lt; .5 miles or Sky layer base &lt; 200</Title>
           <ogc:Filter>
             <ogc:Or>
            <ogc:PropertyIsLessThan>
             <ogc:PropertyName>visibility</ogc:PropertyName>
             <ogc:Literal>.5</ogc:Literal>
            </ogc:PropertyIsLessThan>
             <ogc:PropertyIsLessThan>
             <ogc:PropertyName>skyLayerBase</ogc:PropertyName>
             <ogc:Literal>200</ogc:Literal>
            </ogc:PropertyIsLessThan>
             </ogc:Or>
          </ogc:Filter>
              <PointSymbolizer>
                <Graphic>
                  <Mark>
                    <WellKnownName>square</WellKnownName>
                    <Fill>
                      <CssParameter name="fill">#FF0000</CssParameter>
                    </Fill>
                    <Stroke>
                      <CssParameter name="stroke">#000000</CssParameter>
                      <CssParameter name="stroke-width">1</CssParameter>
                    </Stroke>
                  </Mark>
                <Size>6</Size>
              </Graphic>
          </PointSymbolizer>
        </Rule>
       
        <Rule>
          <Name>rule4</Name>
          <Title>Vis between .5 and 1 or Sky layer base between 200 and 300</Title>
         <ogc:Filter>
             <ogc:Or>
              <ogc:PropertyIsBetween>
              <ogc:PropertyName>visibility</ogc:PropertyName>
              <ogc:LowerBoundary>
                <ogc:Literal>.5</ogc:Literal>
              </ogc:LowerBoundary>
              <ogc:UpperBoundary>
                <ogc:Literal>1</ogc:Literal>
              </ogc:UpperBoundary>
            </ogc:PropertyIsBetween>
              <ogc:PropertyIsBetween>
              <ogc:PropertyName>skyLayerBase</ogc:PropertyName>
              <ogc:LowerBoundary>
                <ogc:Literal>200</ogc:Literal>
              </ogc:LowerBoundary>
              <ogc:UpperBoundary>
                <ogc:Literal>300</ogc:Literal>
              </ogc:UpperBoundary>
            </ogc:PropertyIsBetween>
             </ogc:Or>
          </ogc:Filter>
              <PointSymbolizer>
                <Graphic>
                  <Mark>
                    <WellKnownName>square</WellKnownName>
                    <Fill>
                      <CssParameter name="fill">#FF8000</CssParameter>
                    </Fill>
                    <Stroke>
                      <CssParameter name="stroke"> #000000</CssParameter>
                      <CssParameter name="stroke-width">1</CssParameter>
                    </Stroke>
                  </Mark>
                <Size>6</Size>
              </Graphic>
          </PointSymbolizer>
        </Rule>
            <Rule>
          <Name>rule3</Name>
          <Title>Vis between 1 and 2.25 or Sky layer base between 300 and 700</Title>
          <ogc:Filter>
             <ogc:Or>
                 <ogc:PropertyIsBetween>
                  <ogc:PropertyName>visibility</ogc:PropertyName>
                  <ogc:LowerBoundary>
                    <ogc:Literal>1</ogc:Literal>
                  </ogc:LowerBoundary>
                  <ogc:UpperBoundary>
                    <ogc:Literal>2.25</ogc:Literal>
                  </ogc:UpperBoundary>
                </ogc:PropertyIsBetween>
                  <ogc:PropertyIsBetween>
                  <ogc:PropertyName>skyLayerBase</ogc:PropertyName>
                  <ogc:LowerBoundary>
                    <ogc:Literal>300</ogc:Literal>
                  </ogc:LowerBoundary>
                  <ogc:UpperBoundary>
                    <ogc:Literal>700</ogc:Literal>
                  </ogc:UpperBoundary>
                </ogc:PropertyIsBetween>
             </ogc:Or>
          </ogc:Filter>
              <PointSymbolizer>
                <Graphic>
                  <Mark>
                    <WellKnownName>square</WellKnownName>
                    <Fill>
                      <CssParameter name="fill">#FFFF00</CssParameter>
                    </Fill>
                    <Stroke>
                      <CssParameter name="stroke"> #000000</CssParameter>
                      <CssParameter name="stroke-width">1</CssParameter>
                    </Stroke>
                  </Mark>
                <Size>6</Size>
              </Graphic>
          </PointSymbolizer>
        </Rule>
           
        <Rule>
          <Name>rule2</Name>
          <Title>Vis between 2.26 and 3.8 or Sky layer base between 700 and 1500</Title>
         <ogc:Filter>
            <ogc:Or>
            <ogc:PropertyIsBetween>
              <ogc:PropertyName>visibility</ogc:PropertyName>
              <ogc:LowerBoundary>
                <ogc:Literal>2.25</ogc:Literal>
              </ogc:LowerBoundary>
              <ogc:UpperBoundary>
                <ogc:Literal>3.8</ogc:Literal>
              </ogc:UpperBoundary>
            </ogc:PropertyIsBetween>
              <ogc:PropertyIsBetween>
              <ogc:PropertyName>skyLayerBase</ogc:PropertyName>
              <ogc:LowerBoundary>
                <ogc:Literal>700</ogc:Literal>
              </ogc:LowerBoundary>
              <ogc:UpperBoundary>
                <ogc:Literal>1500</ogc:Literal>
              </ogc:UpperBoundary>
            </ogc:PropertyIsBetween>
          </ogc:Or>
          </ogc:Filter>
              <PointSymbolizer>
                <Graphic>
                  <Mark>
                    <WellKnownName>square</WellKnownName>
                    <Fill>
                      <CssParameter name="fill">#00FF00</CssParameter>
                    </Fill>
                    <Stroke>
                      <CssParameter name="stroke"> #000000</CssParameter>
                      <CssParameter name="stroke-width">1</CssParameter>
                    </Stroke>
                  </Mark>
                <Size>6</Size>
              </Graphic>
          </PointSymbolizer>
          </Rule>

        
        <Rule>
          <Name>rule1</Name>
          <Title>Vis between 3.8 and 5 or Sky layer base between 1500 and 2500</Title>
          <ogc:Filter>
            <ogc:Or>
            <ogc:PropertyIsBetween>
              <ogc:PropertyName>visibility</ogc:PropertyName>
              <ogc:LowerBoundary>
                <ogc:Literal>3.8</ogc:Literal>
              </ogc:LowerBoundary>
              <ogc:UpperBoundary>
                <ogc:Literal>5</ogc:Literal>
              </ogc:UpperBoundary>
            </ogc:PropertyIsBetween>
              <ogc:PropertyIsBetween>
              <ogc:PropertyName>skyLayerBase</ogc:PropertyName>
              <ogc:LowerBoundary>
                <ogc:Literal>1500</ogc:Literal>
              </ogc:LowerBoundary>
              <ogc:UpperBoundary>
                <ogc:Literal>2500</ogc:Literal>
              </ogc:UpperBoundary>
            </ogc:PropertyIsBetween>
          </ogc:Or>
          </ogc:Filter>
           <PointSymbolizer>
              <Graphic>
                <Mark>
                  <WellKnownName>square</WellKnownName>
                  <Fill>
                    <CssParameter name="fill">#FFFFFF</CssParameter>
                  </Fill>
                  <Stroke>
                    <CssParameter name="stroke"> #000000</CssParameter>
                    <CssParameter name="stroke-width">1</CssParameter>
                  </Stroke>
                </Mark>
              <Size>6</Size>
            </Graphic>
          </PointSymbolizer>
        </Rule>
        
         <Rule>
           <Name>rule0</Name>
          <Title>Vis &gt; 5 or Sky layer &gt; 2500</Title>
           <ogc:Filter>
             <ogc:And>
                 <ogc:Or>
                    <ogc:PropertyIsGreaterThan>
                     <ogc:PropertyName>visibility</ogc:PropertyName>
                     <ogc:Literal>5</ogc:Literal>
                    </ogc:PropertyIsGreaterThan>
                      <ogc:PropertyIsEqualTo>
                     <ogc:PropertyName>visibility</ogc:PropertyName>
                     <ogc:Literal>-9999</ogc:Literal>
                   </ogc:PropertyIsEqualTo>
                 </ogc:Or>
                 <ogc:Or>
                     <ogc:PropertyIsGreaterThan>
                     <ogc:PropertyName>skyLayerBase</ogc:PropertyName>
                     <ogc:Literal>2500</ogc:Literal>
                    </ogc:PropertyIsGreaterThan>
                       <ogc:PropertyIsEqualTo>
                           <ogc:PropertyName>skyLayerBase</ogc:PropertyName>
                           <ogc:Literal>-9999</ogc:Literal>
                         </ogc:PropertyIsEqualTo>
                 </ogc:Or>
             </ogc:And>
             
          </ogc:Filter>
              <PointSymbolizer>
                <Graphic>
                  <Mark>
                    <WellKnownName>square</WellKnownName>
                    <Fill>
                      <CssParameter name="fill">#00F5FF</CssParameter>
                    </Fill>
                    <Stroke>
                      <CssParameter name="stroke"> #000000</CssParameter>
                      <CssParameter name="stroke-width">1</CssParameter>
                    </Stroke>
                  </Mark>
                <Size>6</Size>
              </Graphic>
          </PointSymbolizer>
        </Rule>
        
        
       
       
       <Rule>
          <Name>rulelabel</Name>         
          <Title>label</Title>
          <Abstract>A label goes on the point</Abstract>
          
           <TextSymbolizer>
                  <Label>
                    <ogc:PropertyName>stationId</ogc:PropertyName>
                  </Label>
                  <Font>
                    <CssParameter name="font-family">Arial</CssParameter>
                    <CssParameter name="font-weight">Bold</CssParameter>
                    <CssParameter name="font-style">
                        <literal>Normal</literal>
                    </CssParameter>
                    <CssParameter name="font-size">10</CssParameter>
                  </Font>
                   <LabelPlacement>
                <PointPlacement>
                  <AnchorPoint>
                    <AnchorPointX>0.5</AnchorPointX>
                    <AnchorPointY>0.5</AnchorPointY>
                  </AnchorPoint>
                  <Displacement>
                    <DisplacementX>5</DisplacementX>
                    <DisplacementY>-15</DisplacementY>
                  </Displacement>
                </PointPlacement>
              </LabelPlacement>
              <Fill>
                <CssParameter name="fill">#000000</CssParameter>
              </Fill>
           </TextSymbolizer>
        </Rule> 
  
      </FeatureTypeStyle>
    </UserStyle>
  </NamedLayer>
</StyledLayerDescriptor>