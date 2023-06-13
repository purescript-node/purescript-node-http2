export const getDefaultSettings = () => http2.getDefaultSettings();
export const getPackedSettingsImpl = (settings) => http2.getPackedSettings(settings);
export const getUnpackedSettingsImpl = (buf) => http2.getUnpackedSettings(buf);
