import http2 from "node:http2";

export const createSecureServerImpl = (opts) => http2.createSecureServer(opts);
export const connectAuthImpl = (auth) => http2.connect(auth);
export const connectAuthOptionsImpl = (auth) => http2.connect(auth, options);
export const getDefaultSettings = () => http2.getDefaultSettings();
export const getPackedSettingsImpl = (settings) => http2.getPackedSettings(settings);
export const getUnpackedSettingsImpl = (buf) => http2.getUnpackedSettings(buf);

const undefined_ = undefined;
export { undefined_ as undefined }
