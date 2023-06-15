import http2 from "node:http2";

export const createSecureServerImpl = (opts) => http2.createSecureServer(opts);
export const setTimeoutImpl = (s, msecs) => s.setTimeout(msecs);
export const timeoutImpl = (s) => s.timeout;
export const updateSettingsImpl = (s, settings) => s.updateSettings(settings);
