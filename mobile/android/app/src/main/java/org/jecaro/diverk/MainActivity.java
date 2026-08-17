package org.jecaro.diverk;

import android.os.Build;
import android.webkit.WebView;
import com.getcapacitor.BridgeActivity;

public class MainActivity extends BridgeActivity {

    @Override
    protected void onCreate(android.os.Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            getOnBackInvokedDispatcher().registerOnBackInvokedCallback(
                android.window.OnBackInvokedDispatcher.PRIORITY_OVERLAY,
                this::handleBack
            );
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
