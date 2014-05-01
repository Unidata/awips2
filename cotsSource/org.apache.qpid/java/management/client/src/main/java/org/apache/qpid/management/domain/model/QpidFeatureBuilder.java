/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid.management.domain.model;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.management.MBeanAttributeInfo;
import javax.management.MBeanFeatureInfo;
import javax.management.MBeanOperationInfo;
import javax.management.MBeanParameterInfo;

import org.apache.qpid.management.configuration.Configuration;
import org.apache.qpid.management.configuration.UnknownTypeCodeException;
import org.apache.qpid.management.domain.handler.impl.MethodOrEventDataTransferObject;
import org.apache.qpid.management.Names;

/**
 * A builder used to parse incoming schema message and therefore to build a feature (property, statistic, method, event)
 * definition.
 * In order to set up the correct state for this builder, clients must create an instance of this class
 * The product of the builder will be a QpidFeature and a  JMX Managemtn feature used for describing that feature in a 
 * JMX environment. So, for example, for building a property definition client code should be :
 * 
 * <br>- QpidFeatureBuilder builder = QpidFeature.createPropertyBuilder(...);
 * <br>- builder.build();
 * <br>- QpidProperty property = (QpidProperty) builder.getQpidFeature();
 * <br>- MBeanAttributeInfo managementAttributeInfo = (MBeanAttributeInfo)builder.getManagementFeature();
 * 
 * <br>N.B.: a builder instance is not supposed to be reused. One instance for one feature!
 * 
 * @author Andrea Gazzarini
 */
class QpidFeatureBuilder
{
    
    static enum Attribute {
        name,type,access,index,optional,unit,min,max,maxlen,desc,dir,argCount;
    };
    
    private List<Attribute> _mandatoryAttributes = new ArrayList<Attribute>();
    
    /**
     * Builder state for this class.
     * Each concrete implementor is a builder for a specific feature. 
     * using the appropriate factory method.
     * 
     * @author Andrea Gazzarini
     */
    interface State {
        void build() throws UnableToBuildFeatureException;
    }
    
    /**
     * Builder used for building property definition.
     */
    final State _propertyBuilder = new State() {

        /**
         * Builds a property definition as well a management attribute feature.
         */
        public void build () throws UnableToBuildFeatureException
        {
            QpidProperty property = new QpidProperty();
            try {         
                int optionalIndex = 0;
                for (Entry<String, Object> propertyAttribute : _featureDefinition.entrySet())
                {
                    Attribute attribute = Attribute.valueOf(propertyAttribute.getKey());
                    switch(attribute)
                    {
                        case name : 
                        {
                            property.setName(String.valueOf(propertyAttribute.getValue()));       
                            break;
                        }
                        case access : 
                        {
                            int code = (Integer)propertyAttribute.getValue();
                            property.setAccessMode(Configuration.getInstance().getAccessMode(code)); 
                            break;
                        }
                        case unit :
                        {
                            property.setUnit(String.valueOf(propertyAttribute.getValue()));
                            break;                        
                        }
                        case min :
                        {
                            property.setMinValue((Integer)propertyAttribute.getValue());
                            break;                        
                        }
                        case max :
                        {
                            property.setMaxValue((Integer)propertyAttribute.getValue());
                            break;                        
                        }
                        case maxlen :
                        {
                            property.setMaxLength((Integer)propertyAttribute.getValue());
                            break;                        
                        }
                        case desc :
                        {
                            property.setDescription(String.valueOf(propertyAttribute.getValue()));
                            break;                        
                        }
                        case type : 
                        {
                            int code = (Integer) propertyAttribute.getValue();
                            property.setType(Configuration.getInstance().getType(code));
                            break;
                        }
                        case index : 
                        {
                            break;
                        }
                        case optional : 
                        {
                            int code = (Integer) propertyAttribute.getValue();
                            if (code == 1)
                            {
                                property.markAsOptional(optionalIndex);
                                optionalIndex++;
                            } 
                            break;
                        }                    
                    }
                    _mandatoryAttributes.remove(attribute);
                }                
            } catch(Exception exception) 
            {
                throw new UnableToBuildFeatureException(exception,property.getName());
            } 
            
            if (!_mandatoryAttributes.isEmpty())
            {
                throw new MissingFeatureAttributesException(_mandatoryAttributes);
            }
            
            _managementFeatureInfo = new MBeanAttributeInfo(
                    property.getName(),
                    property.getJavaType().getName(),
                    property.getDescription(),
                    true,
                    property.getAccessMode()==AccessMode.RW,
                    false);
            _qpidFeature = property;
        }
    };
    
    final State _statisticBuilder = new State()
    {
        public void build () throws UnableToBuildFeatureException
        {
            QpidStatistic statistic = new QpidStatistic();
            try 
            {
                for (Entry<String, Object> statisticAttribute : _featureDefinition.entrySet())
                {
                    Attribute attribute = Attribute.valueOf(statisticAttribute.getKey());
                    switch(attribute)
                    {
                        case name : 
                        {
                            statistic.setName(String.valueOf(statisticAttribute.getValue()));
                            break;
                        }
                        case unit :
                        {
                            statistic.setUnit(String.valueOf(statisticAttribute.getValue()));
                            break;                        
                        }
                        case desc :
                        {
                            statistic.setDescription(String.valueOf(statisticAttribute.getValue()));
                            break;                        
                        }
                        case type : 
                        {
                            int code = (Integer) statisticAttribute.getValue();
                            statistic.setType(Configuration.getInstance().getType(code));
                            break;
                        }
                   }
                    _mandatoryAttributes.remove(attribute);
                }
            } catch(Exception exception) 
            {
                throw new UnableToBuildFeatureException(exception,statistic.getName());
            }            
            
            if (!_mandatoryAttributes.isEmpty())
            {
                throw new MissingFeatureAttributesException(_mandatoryAttributes);
            }
            
            _managementFeatureInfo = new MBeanAttributeInfo(
                    statistic.getName(),
                    statistic.getJavaType().getName(),
                    statistic.getDescription(),
                    true,
                    false,
                    false);
            _qpidFeature = statistic;
        }
    };
        
    /**
     * Builder used for building a statistic definition.
     */
    final State _argumentBuilder = new State()
    {
        /**
         * Builds a property definition as well a management attribute feature.
         */
        public void build () throws UnableToBuildFeatureException
        {
            QpidArgument argument = new QpidArgument();
            for (Entry<String, Object> argumentAttribute : _featureDefinition.entrySet())
            {
                String key = argumentAttribute.getKey();
                if (Names.DEFAULT_PARAM_NAME.equals(key))
                {
                    argument.setDefaultValue(argumentAttribute.getValue());
                } else {
                    Attribute attribute = Attribute.valueOf(key);
                    switch (attribute)
                    {
                        case name :
                        {
                            argument.setName((String)argumentAttribute.getValue());
                            break;
                        }
                        case desc :
                        {
                            argument.setDescription((String)argumentAttribute.getValue());
                            break;
                        }
                        case type :
                        {
                            try 
                            {
                                argument.setType(Configuration.getInstance().getType((Integer)argumentAttribute.getValue()));
                                break;
                            } catch(UnknownTypeCodeException exception)
                            {
                                throw new UnableToBuildFeatureException(exception,argument.getName());                                
                            }
                        }
                        case dir : 
                        {
                            argument.setDirection((String)argumentAttribute.getValue());                            
                            break;
                        }
                        case unit : 
                        {
                            argument.setUnit((String)argumentAttribute.getValue());
                            break;
                            
                        }
                    }
                }
            }
            
            if (!_mandatoryAttributes.isEmpty())
            {
                throw new MissingFeatureAttributesException(_mandatoryAttributes);
            }
            
            _qpidFeature = argument;
            _managementFeatureInfo = new MBeanParameterInfo(
                    argument.getName(), 
                    argument.getJavaType().getName(),
                    argument.getDescription());            
        }  
    };
    
    final State _methodBuilder = new State()
    {
        public void build () throws UnableToBuildFeatureException
        {
            Map<String,Object> definition = _methodOrEventDefinition.getDefinition();
            String name = (String)definition.get(Attribute.name.name());
            if (name == null)
            {
                throw new MissingFeatureAttributesException(_mandatoryAttributes);
            }
            
            QpidMethod method = new QpidMethod((String)definition.get(Attribute.name.name()),(String) definition.get(Attribute.desc.name()));

            List<Map<String,Object>> args = _methodOrEventDefinition.getArgumentsDefinitions();            
            
            List<MBeanParameterInfo> signature = new LinkedList<MBeanParameterInfo>();
            
            for (Map<String,Object> argumentDefinition : args)
            {
                QpidFeatureBuilder builder = QpidFeatureBuilder.createArgumentBuilder(argumentDefinition);
                builder.build();

                QpidArgument argument = (QpidArgument) builder.getQpidFeature();
                method.addArgument(argument);
                if (argument.isInput())
                {
                    signature.add((MBeanParameterInfo) builder.getManagementFeature());
                }
            }    

            _qpidFeature = method;
            _managementFeatureInfo = new MBeanOperationInfo(
                  method.getName(),
                  method.getDescription(),
                  (MBeanParameterInfo[])signature.toArray(new MBeanParameterInfo[signature.size()]),
                  void.class.getName(),
                  MBeanOperationInfo.ACTION);
        }
    };

    final State _eventBuilder = new State()
    {
        public void build () throws UnableToBuildFeatureException
        {
        }
    };
    
    private MBeanFeatureInfo _managementFeatureInfo;
    private QpidFeature _qpidFeature;
    private final Map <String, Object> _featureDefinition;
    private final MethodOrEventDataTransferObject _methodOrEventDefinition;
    private State _state;
    
    static QpidFeatureBuilder createPropertyBuilder(Map<String, Object> propertyDefinition)
    {
        QpidFeatureBuilder result = new QpidFeatureBuilder(propertyDefinition);
        result._state = result._propertyBuilder;
        result._mandatoryAttributes.add(Attribute.name);
        result._mandatoryAttributes.add(Attribute.access);
        result._mandatoryAttributes.add(Attribute.type);
        result._mandatoryAttributes.add(Attribute.optional);
        result._mandatoryAttributes.add(Attribute.index);
        return result;
    }

    static QpidFeatureBuilder createStatisticBuilder(Map<String, Object> statisticDefinition)
    {
        QpidFeatureBuilder result = new QpidFeatureBuilder(statisticDefinition);
        result._state = result._statisticBuilder;
        result._mandatoryAttributes.add(Attribute.name);
        result._mandatoryAttributes.add(Attribute.type);
        return result;
    }

    static QpidFeatureBuilder createEventBuilder(Map<String, Object> eventDefinition)
    {
        QpidFeatureBuilder result = new QpidFeatureBuilder(eventDefinition);
        result._state = result._eventBuilder;
        return result;
    }

    static QpidFeatureBuilder createMethodBuilder(MethodOrEventDataTransferObject methodDefinition)
    {
        QpidFeatureBuilder result = new QpidFeatureBuilder(methodDefinition);
        result._state = result._methodBuilder;
        result._mandatoryAttributes.add(Attribute.name); 
        return result;
    }

    private static QpidFeatureBuilder createArgumentBuilder(Map<String, Object> argumentDefinition)
    {
        QpidFeatureBuilder result = new QpidFeatureBuilder(argumentDefinition);
        result._state = result._argumentBuilder;
        return result;
    }

    
    /**
     * Builds new builder with the given data.
     * This constructor is used for building properties, statistics and arguments.
     * 
     * @param definition the feature definition data.
     */
    private QpidFeatureBuilder(Map<String, Object> definition) 
    {
        this._featureDefinition = definition;
        this._methodOrEventDefinition = null;
    }    

    /**
     * Builds new builder with the given data.
     * This constructor is used for building properties, statistics and arguments.
     * 
     * @param definition the feature definition data.
     */
    private QpidFeatureBuilder(MethodOrEventDataTransferObject definition) 
    {
        this._featureDefinition = null;
        this._methodOrEventDefinition = definition;
    }    
    
    /**
     * Returns the just built qpid feature.
     *  
     * @return the qpid feature.
     */
    QpidFeature getQpidFeature() 
    {
        return _qpidFeature;
    }
    
    /**
     * Return the jmx metadata for the built feature.
     * 
     * @return the jmx metadata for the built feature.
     */
    MBeanFeatureInfo getManagementFeature()
    {
        return _managementFeatureInfo;
    }
    
    void build() throws UnableToBuildFeatureException
    {
    	try 
    	{
    		_state.build();
    	} catch(UnableToBuildFeatureException exception)
    	{
    		throw exception;
    	} catch(Exception exception)
    	{
    		throw new UnableToBuildFeatureException(exception,"Feature name is not available for debugging.");
    	}
    }
}
