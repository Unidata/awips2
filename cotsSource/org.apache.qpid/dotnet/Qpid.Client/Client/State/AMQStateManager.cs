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
using System.Collections;
using log4net;
using Apache.Qpid.Client.Handler;
using Apache.Qpid.Client.Protocol;
using Apache.Qpid.Client.Protocol.Listener;
using Apache.Qpid.Framing;

namespace Apache.Qpid.Client.State
{
    public class AMQStateManager : IAMQMethodListener
    {
        private static readonly ILog _logger = LogManager.GetLogger(typeof(AMQStateManager));

        const bool InfoLoggingHack = true;
            
        /// <summary>
        /// The current state
        /// </summary>
        private AMQState _currentState;

        /// <summary>
        /// Maps from an AMQState instance to a Map from Class to StateTransitionHandler.
        /// The class must be a subclass of AMQFrame.
        /// </summary>
        private readonly IDictionary _state2HandlersMap;
        private ArrayList _stateListeners;
        private object _syncLock;
        
        public AMQStateManager()
        {
            _syncLock = new object();
            _state2HandlersMap = new Hashtable();
            _stateListeners = ArrayList.Synchronized(new ArrayList(5));
            _currentState = AMQState.CONNECTION_NOT_STARTED;
            RegisterListeners();
        }

        private void RegisterListeners()
        {
            IStateAwareMethodListener connectionStart = new ConnectionStartMethodHandler();
            IStateAwareMethodListener connectionClose = new ConnectionCloseMethodHandler();
            IStateAwareMethodListener connectionCloseOk = new ConnectionCloseOkHandler();
            IStateAwareMethodListener connectionTune = new ConnectionTuneMethodHandler();
            IStateAwareMethodListener connectionSecure = new ConnectionSecureMethodHandler();
            IStateAwareMethodListener connectionOpenOk = new ConnectionOpenOkMethodHandler();
            IStateAwareMethodListener channelClose = new ChannelCloseMethodHandler();
            IStateAwareMethodListener basicDeliver = new BasicDeliverMethodHandler();
            IStateAwareMethodListener basicReturn = new BasicReturnMethodHandler();
            IStateAwareMethodListener queueDeleteOk = new QueueDeleteOkMethodHandler();
            IStateAwareMethodListener queuePurgeOk = new QueuePurgeOkMethodHandler();
            
            // We need to register a map for the null (i.e. all state) handlers otherwise you get
            // a stack overflow in the handler searching code when you present it with a frame for which
            // no handlers are registered.
            _state2HandlersMap[AMQState.ALL] = new Hashtable();

            {
                Hashtable notStarted = new Hashtable();
                notStarted[typeof(ConnectionStartBody)] = connectionStart;
                notStarted[typeof(ConnectionCloseBody)] = connectionClose;
                _state2HandlersMap[AMQState.CONNECTION_NOT_STARTED] = notStarted;
            }
            {
                Hashtable notTuned = new Hashtable();
                notTuned[typeof(ConnectionTuneBody)] = connectionTune;
                notTuned[typeof(ConnectionSecureBody)] = connectionSecure;
                notTuned[typeof(ConnectionCloseBody)] = connectionClose;
                _state2HandlersMap[AMQState.CONNECTION_NOT_TUNED] = notTuned;
            }
            {
                Hashtable notOpened = new Hashtable();
                notOpened[typeof(ConnectionOpenOkBody)] = connectionOpenOk;
                notOpened[typeof(ConnectionCloseBody)] = connectionClose;
                _state2HandlersMap[AMQState.CONNECTION_NOT_OPENED] = notOpened;
            }
            {
                Hashtable open = new Hashtable();
                open[typeof(ChannelCloseBody)] = channelClose;
                open[typeof(ConnectionCloseBody)] = connectionClose;
                open[typeof(BasicDeliverBody)] = basicDeliver;
                open[typeof(BasicReturnBody)] = basicReturn;
                open[typeof(QueueDeleteOkBody)] = queueDeleteOk;
                open[typeof(QueuePurgeOkBody)] = queuePurgeOk;
                _state2HandlersMap[AMQState.CONNECTION_OPEN] = open;
            }
            {
                Hashtable closing = new Hashtable();
                closing[typeof(ConnectionCloseOkBody)] = connectionCloseOk;
                _state2HandlersMap[AMQState.CONNECTION_CLOSING] = closing;
            }
        }

        public AMQState CurrentState
        {
            get
            {
                return _currentState;
            }
        }

        /// <summary>
        /// Changes the state.
        /// </summary>
        /// <param name="newState">The new state.</param>
        /// <exception cref="AMQException">if there is an error changing state</exception>
        public void ChangeState(AMQState newState)
        {
            if (InfoLoggingHack)
            {
                _logger.Debug("State changing to " + newState + " from old state " + _currentState);
            }
            _logger.Debug("State changing to " + newState + " from old state " + _currentState);
            AMQState oldState = _currentState;
            _currentState = newState;

            lock ( _syncLock )
            {
               foreach ( IStateListener l in _stateListeners )
               {
                  l.StateChanged(oldState, newState);
               }
            }
        }

        public void Error(Exception e)
        {
            _logger.Debug("State manager receive error notification: " + e);
            lock ( _syncLock )
            {
               foreach ( IStateListener l in _stateListeners )
               {
                  l.Error(e);
               }
            }
        }

        public bool MethodReceived(AMQMethodEvent evt)
        {
            _logger.Debug(String.Format("Finding method handler. currentState={0} type={1}", _currentState, evt.Method.GetType()));            
            IStateAwareMethodListener handler = FindStateTransitionHandler(_currentState, evt.Method);
            if (handler != null)
            {
                handler.MethodReceived(this, evt);
                return true;
            }
            return false;
        }

        /// <summary>
        /// Finds the state transition handler.
        /// </summary>
        /// <param name="currentState">State of the current.</param>
        /// <param name="frame">The frame.</param>
        /// <returns></returns>
        /// <exception cref="IllegalStateTransitionException">if the state transition if not allowed</exception>
        private IStateAwareMethodListener FindStateTransitionHandler(AMQState currentState,
                                                                     AMQMethodBody frame)                
        {
            Type clazz = frame.GetType();
            if (_logger.IsDebugEnabled)
            {
                _logger.Debug("Looking for state transition handler for frame " + clazz);
            }
            IDictionary classToHandlerMap = (IDictionary) _state2HandlersMap[currentState];

            if (classToHandlerMap == null)
            {
                // if no specialised per state handler is registered look for a
                // handler registered for "all" states
                return FindStateTransitionHandler(AMQState.ALL, frame);
            }
            IStateAwareMethodListener handler = (IStateAwareMethodListener) classToHandlerMap[clazz];
            if (handler == null)
            {
                if (currentState == AMQState.ALL)
                {
                    _logger.Debug("No state transition handler defined for receiving frame " + frame);
                    return null;
                }
                else
                {
                    // if no specialised per state handler is registered look for a
                    // handler registered for "all" states
                    return FindStateTransitionHandler(AMQState.ALL, frame);
                }
            }
            else
            {
                return handler;
            }
        }

        public void AddStateListener(IStateListener listener)
        {
            _logger.Debug("Adding state listener");
            lock ( _syncLock )
            {
               _stateListeners.Add(listener);
            }
        }

        public void RemoveStateListener(IStateListener listener)
        {
           lock ( _syncLock )
           {
              _stateListeners.Remove(listener);
           }
        }

        public void AttainState(AMQState s)
        {
            if (_currentState != s)
            {
                StateWaiter sw = null;
                try
                {
                    _logger.Debug("Adding state wait to reach state " + s);
                    sw = new StateWaiter(s);
                    AddStateListener(sw);
                    sw.WaituntilStateHasChanged();
                    // at this point the state will have changed.
                }
                finally
                { 
                    RemoveStateListener(sw);
                }
            }
        }        
    }
}
