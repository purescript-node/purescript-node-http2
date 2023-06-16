import { constants } from "node:http2";

export const noError = constants.NGHTTP2_NO_ERROR;
export const protocolError = constants.NGHTTP2_PROTOCOL_ERROR;
export const internalError = constants.NGHTTP2_INTERNAL_ERROR;
export const flowControlError = constants.NGHTTP2_FLOW_CONTROL_ERROR;
export const settingsTimeout = constants.NGHTTP2_SETTINGS_TIMEOUT;
export const streamClosed = constants.NGHTTP2_STREAM_CLOSED;
export const frameSizeError = constants.NGHTTP2_FRAME_SIZE_ERROR;
export const refusedStream = constants.NGHTTP2_REFUSED_STREAM;
export const cancel = constants.NGHTTP2_CANCEL;
export const compressionError = constants.NGHTTP2_COMPRESSION_ERROR;
export const connectError = constants.NGHTTP2_CONNECT_ERROR;
export const enhanceYourCalm = constants.NGHTTP2_ENHANCE_YOUR_CALM;
export const inadequateSecurity = constants.NGHTTP2_INADEQUATE_SECURITY;
export const http1_1Required = constants.NGHTTP2_HTTP_1_1_REQUIRED;
