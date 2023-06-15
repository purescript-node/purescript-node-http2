import http2 from "node:http2";

export const connectAuthImpl = (auth) => http2.connect(auth);
export const connectAuthOptionsImpl = (auth, options) => http2.connect(auth, options);
