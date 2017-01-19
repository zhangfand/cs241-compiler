package edu.zhangfan.compiler.frontend;

import org.junit.Test;

import java.io.*;
import java.nio.charset.StandardCharsets;


public class ScannerTest {
    private Scanner scanner;

    @Test
    public void smokeTest() throws Exception {
        String exampleCode = "x != 1";
        InputStream stream = new ByteArrayInputStream(exampleCode.getBytes(StandardCharsets.UTF_8));
        SourceReader reader = new SourceReader(stream);
        Scanner scanner = new Scanner(reader);

        while (scanner.hasNext()) {
            Token token = scanner.peek();
            System.out.println(token);
            scanner.next();
        }
    }

}