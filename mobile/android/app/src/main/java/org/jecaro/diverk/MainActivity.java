package org.jecaro.diverk;

import android.os.Build;
import android.webkit.WebResourceRequest;
import android.webkit.WebResourceResponse;
import android.webkit.WebView;
import com.getcapacitor.BridgeActivity;
import com.getcapacitor.BridgeWebViewClient;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class MainActivity extends BridgeActivity {

    private static final Set<String> ALLOWED_HEADERS = new HashSet<>(
        Arrays.asList("Authorization", "Accept", "Content-Type", "User-Agent"));

    @Override
    protected void onCreate(android.os.Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            getOnBackInvokedDispatcher().registerOnBackInvokedCallback(
                android.window.OnBackInvokedDispatcher.PRIORITY_OVERLAY,
                this::handleBack
            );
        }
        // Proxy /api/github/* → https://api.github.com/* natively, bypassing WebView CORS.
        bridge.getWebView().setWebViewClient(new BridgeWebViewClient(bridge) {
            @Override
            public WebResourceResponse shouldInterceptRequest(WebView view, WebResourceRequest request) {
                String path = request.getUrl().getPath();
                if (path != null && path.startsWith("/api/github/")) {
                    return proxyToGitHub(request, path);
                }
                return super.shouldInterceptRequest(view, request);
            }
        });
    }

    private WebResourceResponse proxyToGitHub(WebResourceRequest request, String path) {
        try {
            String githubPath = path.substring("/api/github".length());
            String query = request.getUrl().getQuery();
            String urlStr = "https://api.github.com" + githubPath
                + (query != null ? "?" + query : "");

            HttpURLConnection conn = (HttpURLConnection) new URL(urlStr).openConnection();
            conn.setConnectTimeout(10_000);
            conn.setReadTimeout(15_000);
            for (Map.Entry<String, String> h : request.getRequestHeaders().entrySet()) {
                if (ALLOWED_HEADERS.contains(h.getKey())) conn.setRequestProperty(h.getKey(), h.getValue());
            }
            conn.connect();

            int code = conn.getResponseCode();
            String contentType = conn.getContentType();
            Map<String, String> headers = new HashMap<>();
            for (String name : conn.getHeaderFields().keySet()) {
                if (name != null) headers.put(name, conn.getHeaderField(name));
            }
            InputStream body = code >= 400 ? conn.getErrorStream() : conn.getInputStream();
            return new WebResourceResponse(contentType, "utf-8", code, "OK", headers, body);
        } catch (IOException e) {
            return null;
        }
    }

    private void handleBack() {
        WebView webView = bridge != null ? bridge.getWebView() : null;
        if (webView == null) {
            moveTaskToBack(false);
            return;
        }
        if (webView.canGoBack()) {
            webView.goBack();
        } else {
            moveTaskToBack(false);
        }
    }

    @Override
    @SuppressWarnings("deprecation")
    public void onBackPressed() {
        handleBack();
    }
}
