package edu.zhangfan.compiler.frontend;

import java.io.*;

class SourceReader {
    private BufferedReader reader;
    private Character nextChar;

    SourceReader(InputStream in) {
        reader = new BufferedReader(new InputStreamReader(in));
        next();
    }

    Character peek() {
        return nextChar;
    }

    void next() {
        try {
            int nextChar = reader.read();
            if (nextChar < 0) {
                this.nextChar = null;
            } else {
                this.nextChar = (char) nextChar;
            }
        } catch (IOException e) {
            this.nextChar = null;
        }
    }

    boolean hasNext() {
        return nextChar != null;
    }

}

