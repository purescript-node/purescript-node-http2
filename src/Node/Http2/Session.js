export const alpnProtocolImpl = (http2Session) => http2Session.alpnProtocol;
export const closeImpl = (http2Session) => http2Session.close();
// Intentionally not supporting the `http2Session.close(cb)` API
// as it's just `h2s.on("close", cb); h2s.close();`
export const closedImpl = (http2Session) => http2Session.closed;
export const connectingImpl = (http2Session) => http2Session.connecting;
export const destroyImpl = (http2Session) => http2Session.destroy();
export const destroyWithErrorImpl = (http2Session, err) => http2Session.destroy(err);
export const destroyWithCodeImpl = (http2Session, code) => http2Session.destroy(code);
export const destroyWithErrorCodeImpl = (http2Session, code) => http2Session.destroy(err, code);
export const destroyedImpl = (http2Session) => http2Session.destroyed;
export const encryptedImpl = (http2Session) => http2Session.encrypted;
export const goAwayImpl = (http2Session) => http2Session.goAway();
export const goAwayCodeImpl = (http2Session, code) => http2Session.goAway(code);
export const goAwayCodeLastStreamIdImpl = (http2Session, code, lastStreamId) => http2Session.goAway(code, lastStreamId);
export const goAwayCodeLastStreamIdOpaqueDataImpl = (http2Session, code, lastStreamId, opaqueData) => http2Session.goAway(code, lastStreamId, opaqueData);
export const localSettingsImpl = (http2Session) => http2Session.localSettings;
export const originSetImpl = (http2Session) => http2Session.originSet;
export const pendingSettingsAckImpl = (http2Session) => http2Session.pendingSettingsAck;
export const pingImpl = (http2Session, cb) => http2Session.ping(cb);
export const pingPayloadImpl = (http2Session, payload, cb) => http2Session.ping(payload, cb);
export const refImpl = (http2Session) => http2Session.ref();
export const remoteSettingsImpl = (http2Session) => http2Session.remoteSettings;
export const setLocalWindowSizeImpl = (http2Session, windowSize) => http2Session.setLocalWindowSize(windowSize);
export const setTimeoutImpl = (http2Session, msecs, cb) => http2Session.setTimeout(msecs, cb);
export const socketImpl = (http2Session) => http2Session.socket;
export const stateImpl = (http2Session) => http2Session.state;
export const settingsImpl = (http2Session, settings, cb) => http2Session.settings(settings, cb);
export const typeImpl = (http2Session) => http2Session.type;
export const unrefImpl = (http2Session) => http2Session.unref();

// server
export const altsvcOriginImpl = (http2session, alt, origin) => http2session.altsvc(alt, origin);
export const altsvcStreamImpl = (http2session, alt, stream) => http2session.altsvc(alt, stream);
export const originImpl = (http2session, origins) => http2session.origin(origins);

// client
export const requestHeadersImpl = (http2session, headers) => http2session.request(headers);
export const requestHeadersOptionsImpl = (http2session, headers, options) => http2session.request(headers, options);
