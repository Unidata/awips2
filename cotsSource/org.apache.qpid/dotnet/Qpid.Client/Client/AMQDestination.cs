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
using System;

namespace Apache.Qpid.Client
{
    public abstract class AMQDestination
    {
        protected readonly string _exchangeName;
        protected readonly string _exchangeClass;
        protected readonly string _destinationName;
        protected readonly bool _isExclusive;
        protected readonly bool _isAutoDelete;
        protected bool _isDurable;

        public bool IsDurable
        {
           
            get { return _isDurable; }
        }

        protected string _queueName;

        protected AMQDestination(String exchangeName, String exchangeClass, String destinationName, bool isExclusive,
                                 bool isAutoDelete, String queueName)
        {
            // XXX: This is ugly - OnlyRequired because of ReplyToDestination.
//            if (destinationName == null)
//            {
//                throw new ArgumentNullException("destinationName");
//            }

            // XXX: This is ugly - OnlyRequired because of SendingDestinationAdapter.
//            if (exchangeName == null)
//            {
//                throw new ArgumentNullException("exchangeName");
//            }

            // XXX: This is ugly - OnlyRequired because of SendingDestinationAdapter.
//            if (exchangeClass == null)
//            {
//                throw new ArgumentNullException("exchangeClass");
//            }

            _exchangeName = exchangeName;
            _exchangeClass = exchangeClass;
            _destinationName = destinationName;
            _isExclusive = isExclusive;
            _isAutoDelete = isAutoDelete;
            _queueName = queueName;
        }

        public string Name
        {
            get
            {
                return _destinationName;
            }
        }

        public abstract string RoutingKey
        {
            get;
        }

        public abstract string EncodedName
        {
            get;
        }

        public bool AutoDelete
        {
            get
            {
                return _isAutoDelete;
            }
        }

        public string QueueName
        {
            get
            {
                return _queueName;
            }
            set
            {
                _queueName = value;
            }
        }

        public string ExchangeName
        {
            get
            {
                return _exchangeName;
            }
        }

        public string ExchangeClass
        {
            get
            {
                return _exchangeClass;
            }
        }

        public bool IsExclusive
        {
            get
            {
                return _isExclusive;
            }
        }

        public bool IsAutoDelete
        {
            get
            {
                return _isAutoDelete;
            }
        }

        public override string ToString()
        {
            return "Destination: " + _destinationName + ", " +
                   "Queue Name: " + _queueName + ", Exchange: " + _exchangeName +
                   ", Exchange class: " + _exchangeClass + ", Exclusive: " + _isExclusive +
                   ", AutoDelete: " + _isAutoDelete; // +", Routing  Key: " + RoutingKey;
        }

        public override bool Equals(object o)
        {
            if (this == o)
            {
                return true;
            }
            if (o == null || GetType() != o.GetType())
            {
                return false;
            }

            AMQDestination that = (AMQDestination) o;

            if (!StringsNotEqualNullSafe(_destinationName, that._destinationName))
            {
                return false;
            }
            if (!StringsNotEqualNullSafe(_exchangeClass, that._exchangeClass))
            {
                return false;
            }
            if (!StringsNotEqualNullSafe(_exchangeName, that._exchangeName))
            {
                return false;
            }
            if (!StringsNotEqualNullSafe(_queueName, that._queueName))
            {
                return false;
            }
            if (_isExclusive != that._isExclusive)
            {
                return false;
            }
            if (_isAutoDelete != that._isAutoDelete)
            {
                return false;
            }
            return true;
        }

        private bool StringsNotEqualNullSafe(string one, string two)
        {
            if ((one == null && two != null) ||
                (one != null && !one.Equals(two)))
            {
                return false;
            }
            else
            {
                return true;
            }
        }
 
        public override int GetHashCode()
        {
            int result;
            if (_exchangeName == null)
            {
                result = "".GetHashCode();   
            }
            else
            {
                result = _exchangeName.GetHashCode();
            }
            if (_exchangeClass != null)
            {
                result = 29 * result + _exchangeClass.GetHashCode();                
            }
            if (_destinationName != null)
            {
                result = 29 * result + _destinationName.GetHashCode();                
            }
            if (_queueName != null)
            {
                result = 29 * result + _queueName.GetHashCode();
            }
            result = result * (_isExclusive ? 13 : 7);
            result = result * (_isAutoDelete ? 13 : 7);
            
            Console.WriteLine("FIXME HashCode for " + this + " = " + result);
            return result;
        }

        public abstract bool IsNameRequired { get; }
    }
}
