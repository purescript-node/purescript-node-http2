import http2 from "node:http2";

export const createSecureServerImpl = (opts) => http2.createSecureServer(opts);
export const listenImpl = (s, o) => s.listen(o);

const undefined_ = undefined;
export { undefined_ as undefined }

export const onCheckContinueImpl = (s, cb) => s.on("checkContinue", cb);
export const onConnectionImpl = (s, cb) => s.on("connection", cb);
export const onRequestImpl = (s, cb) => s.on("request", cb);
export const onSessionImpl = (s, cb) => s.on("session", cb);
export const onSessionErrorImpl = (s, cb) => s.on("sessionError", cb);
export const onStreamImpl = (s, cb) => s.on("stream", cb);
export const onTimeoutImpl = (s, cb) => s.on("timeout", cb);
export const onUnknownProtocolImpl = (s, cb) => s.on("unknownProtocol", cb);
export const closeImpl = (s) => s.close();
export const setTimeoutImpl = (s, msecs) => s.setTimeout(msecs);
export const timeoutImpl = (s) => s.timeout;
export const updateSettingsImpl = (s, settings) => s.updateSettings(settings);
