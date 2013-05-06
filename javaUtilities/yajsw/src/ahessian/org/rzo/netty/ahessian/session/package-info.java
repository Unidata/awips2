/**
 * Provides Client and Server Session handling
 * <br>
 * When using {@link ChannelPipelineFactory} netty creates a pipeline per connection
 * With sessions we need a pipeline per session. Thus a pipeline may survive multiple connect/disconnect cycles.
 * Sessions thus allows the server to maintain its state in-between disconnect/connect cycles of the client
 * Currently only in-memory sessions are implemented.
 * Therefore when the server is stopped all sessions are lost and reconnecting clients will have to
 * adjust accordingly.
 * 
 * To allow pipeline survival {@link MixinPipeline} is used.
 * On creation of a new session a new {@link MixinPipeline} is created, added to the current pipeline and associated with the session
 * On reconnect, once the session has been identified, the associated {@link MixinPipeline} is added to the
 * current pipeline.
 * 
 * TODO
 * persistent sessions -> handler/pipeline api will have to be extended for persistence
 * distributed sessions using jgroups
 * session timeout -> handlers/pipeline api will have to be extended for cleanup
 */
package org.rzo.netty.ahessian.session;