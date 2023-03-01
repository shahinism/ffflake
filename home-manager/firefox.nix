{pkgs, ...}:
{
  programs.firefox = {
    enable = true;

    profiles.dev-edition-default = {
      path = "p8klfsds.dev-edition-default";
      userChrome = ''
        #TabsToolbar { visibility: collapse;  }
      '';

      search = {
        default = "Startpage.com";
        force = true;
        engines = {
          "Bing".metaData.hidden = true;
          "Google".metaData.hidden = true;
          "Amazon.nl".metaData.hidden = true;
          "Wikipedia (en)".metaData.hidden = true;

          "Startpage.com" = {
            urls = [{template = "https://www.startpage.com/do/dsearch?query={searchTerms}";}];
            iconUpdateURL = "https://www.startpage.com/sp/cdn/favicons/favicon--default.ico";
            updateInterval = 24 * 60 * 60 * 1000; # every day
            definedAliases = ["@sp"];
          };
          
          "Nix Packages" = {
            urls = [
              {
                template = "https://search.nixos.org/packages";
                params = [
                  {
                    name = "type";
                    value = "packages";
                  }
                  {
                    name = "query";
                    value = "{searchTerms}";
                  }
                ];
              }
            ];

            icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
            definedAliases = ["@np"];
          };

          "NixOS Wiki" = {
            urls = [{template = "https://nixos.wiki/index.php?search={searchTerms}";}];
            iconUpdateURL = "https://nixos.wiki/favicon.png";
            updateInterval = 24 * 60 * 60 * 1000; # every day
            definedAliases = ["@nw"];
          };

          "Github" = {
            urls = [
              {template = "https://github.com/search?q{searchTerms}&type=repositories";}
            ];
            iconUpdateURL = "https://github.githubassets.com/favicons/favicon.svg";
            updateInterval = 24 * 60 * 60 * 1000; # every day
            defaultAliases = ["@gh"];
          };
        };
      };

      settings = {
        # Main configuration is based on BetterFox @
        # https://github.com/yokoffing/Betterfox
        ################# FAST FOX #################
        "nglayout.initialpaint.delay" = 0;
        "nglayout.initialpaint.delay_in_oopif" = 0;
        "content.notify.interval" = 100000;
        "browser.startup.preXulSkeletonUI" = false;

        # Experimental
        "layout.css.grid-template-masonry-value.enabled" = true;
        "layout.css.animation-composition.enabled" = true;
        "dom.enable_web_task_scheduling" = true;

        # GFX
        "gfx.webrender.all" = true;
        "gfx.webrender.precache-shaders" = true;
        "gfx.webrender.compositor" = true;
        "layers.gpu-process.enabled" = true;
        "media.hardware-video-decoding.enabled" = true;
        "gfx.canvas.accelerated" = true;
        "gfx.canvas.accelerated.cache-items" = 32768;
        "gfx.canvas.accelerated.cache-size" = 4096;
        "gfx.content.skia-font-cache-size" = 80;
        "image.cache.size" = 10485760;
        "image.mem.decode_bytes_at_a_time" = 131072;
        "image.mem.shared.unmap.min_expiration_ms" = 120000;
        "media.memory_cache_max_size" = 1048576;
        "media.memory_caches_combined_limit_kb" = 2560000;
        "media.cache_readahead_limit" = 9000;
        "media.cache_resume_threshold" = 6000;

        # Browser Cache
        "browser.cache.memory.max_entry_size" = 153600;

        # Network
        "network.buffer.cache.size" = 262144;
        "network.buffer.cache.count" = 128;
        "network.ssl_tokens_cache_capacity" = 32768;
        
        ################# SECUREFOX #################
        # Tracking Protection
        "browser.contentblocking.category" = "strict";
        "privacy.trackingprotection.emailtracking.enabled" = true;
        "urlclassifier.trackingSkipURLs" = "*.reddit.com, *.twitter.com, *.twimg.com, *.tiktok.com";
        "urlclassifier.features.socialtracking.skipURLs" = "*.instagram.com, *.twitter.com, *.twimg.com";
        "privacy.query_stripping.strip_list" = "__hsfp __hssc __hstc __s _hsenc _openstat dclid fbclid gbraid gclid hsCtaTracking igshid mc_eid ml_subscriber ml_subscriber_hash msclkid oft_c oft_ck oft_d oft_id oft_ids oft_k oft_lk oft_sk oly_anon_id oly_enc_id rb_clickid s_cid twclid vero_conv vero_id wbraid wickedid yclid";
        "browser.uitour.enabled" = false;

        # OCSP & CERTS / HPKP
        "security.OCSP.enabled" = 0;
        "security.remote_settings.crlite_filters.enabled" = true;
        "security.pki.crlite_mode" = 2;
        "security.cert_pinning.enforcement_level" = 2;

        # SSL/TLS
        "security.ssl.treat_unsafe_negotiation_as_broken" = true;
        "browser.xul.error_pages.expert_bad_cert" = true;
        "security.tls.enable_0rtt_data" = false;

        # Disk Avoidance
        "browser.cache.disk.enable" = false;
        "browser.privatebrowsing.forceMediaMemoryCache" = true;
        "browser.sessionstore.privacy_level" = 2;

        # Shutdown and Sanitizing
        "privacy.history.custom" = true;

        # Speculative connections
        "network.http.speculative-parallel-limit" = 0;
        "network.dns.disablePrefetch" = true;
        "browser.urlbar.speculativeConnect.enabled" = false;
        "browser.places.speculativeConnect.enabled" = false;
        "network.prefetch-next" = false;
        "network.predictor.enabled" = false;
        "network.predictor.enable-prefetch" = false;

        # Search and URL bar
        "browser.search.separatePrivateDefault.ui.enabled" = true;
        "browser.urlbar.update2.engineAliasRefresh" = true;
        "browser.search.suggest.enabled" = false;
        "browser.urlbar.suggest.quicksuggest.sponsored" = false;
        "browser.urlbar.suggest.quicksuggest.nonsponsored" = false;
        "security.insecure_connection_text.enabled" = true;
        "security.insecure_connection_text.pbmode.enabled" = true;
        "network.IDN_show_punycode" = true;

        # HTTPS-first mode
        "dom.security.https_first" = true;

        # Proxy/Socks/IPv6
        "network.proxy.socks_remote_dns" = true;
        "network.file.disable_unc_paths" = true;
        "network.gio.supported-protocols" = "";

        # Passwords and Autofill
        "signon.formlessCapture.enabled" = false;
        "signon.privateBrowsingCapture.enabled" = false;
        "signon.autofillForms" = false;
        "signon.rememberSignons" = false;
        "editor.truncate_user_pastes" = false;
        "layout.forms.reveal-password-context-menu.enabled" = true;

        # Address + Credit Cards manager
        "extensions.formautofill.addresses.enabled" = false;
        "extensions.formautofill.creditCards.enabled" = false;
        "extensions.formautofill.heuristics.enabled" = false;
        "browser.formfill.enable" = false;

        # Mixed content + Cross-site
        "network.auth.subresource-http-auth-allow" = 1;
        "pdfjs.enableScripting" = false;
        "extensions.postDownloadThirdPartyPrompt" = false;
        "permissions.delegation.enabled" = false;

        # Headers/Referers
        "network.http.referer.XOriginTrimmingPolicy" = 2;

        # Containers
        "privacy.userContext.ui.enabled" = true;

        # Webrtc
        "media.peerconnection.ice.proxy_only_if_behind_proxy" = true;
        "media.peerconnection.ice.default_address_only" = true;

        # Safe Browsing
        "browser.safebrowsing.downloads.remote.enabled" = false;

        # Mozilla
        "accessibility.force_disabled" = 1;
        "identity.fxaccounts.enabled" = false;
        "browser.tabs.firefox-view" = false;
        "permissions.default.desktop-notification" = 2;
        "permissions.default.geo" = 2;
        "geo.provider.network.url" = "https://location.services.mozilla.com/v1/geolocate?key=%MOZILLA_API_KEY%";
        "geo.provider.ms-windows-location" = false; # WINDOWS
        "geo.provider.use_corelocation" = false;  # MAC
        "geo.provider.use_gpsd" = false;        # LINUX
        "geo.provider.use_geoclue" = false;   # LINUX
        "permissions.manager.defaultsUrl" = "";
        "webchannel.allowObject.urlWhitelist" = "";

        # Telemetry
        "toolkit.telemetry.unified" = false;
        "toolkit.telemetry.enabled" = false;
        "toolkit.telemetry.server" = "data:,";
        "toolkit.telemetry.archive.enabled" = false;
        "toolkit.telemetry.newProfilePing.enabled" = false;
        "toolkit.telemetry.shutdownPingSender.enabled" = false;
        "toolkit.telemetry.updatePing.enabled" = false;
        "toolkit.telemetry.bhrPing.enabled" = false;
        "toolkit.telemetry.firstShutdownPing.enabled" = false;
        "toolkit.telemetry.coverage.opt-out" = true;
        "toolkit.coverage.opt-out" = true;
        "datareporting.healthreport.uploadEnabled" = false;
        "datareporting.policy.dataSubmissionEnabled" = false;
        "app.shield.optoutstudies.enabled" = false;
        "browser.discovery.enabled" = false;
        "breakpad.reportURL" = "";
        "browser.tabs.crashReporting.sendReport" = false;
        "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;
        "captivedetect.canonicalURL" = "";
        "network.captive-portal-service.enabled" = false;
        "network.connectivity-service.enabled" = false;
        "default-browser-agent.enabled" = false;
        "app.normandy.enabled" = false;
        "app.normandy.api_url" = "";
        "browser.ping-centre.telemetry" = false;
        "browser.newtabpage.activity-stream.feeds.telemetry" = false;
        "browser.newtabpage.activity-stream.telemetry" = false;

        ################# PESKYFOX #################
        # Mozilla UI
        "layout.css.prefers-color-scheme.content-override" = 2;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "app.update.suppressPrompts" = true;
        "browser.compactmode.show" = true;
        "browser.privatebrowsing.vpnpromourl" = "";
        "extensions.getAddons.showPane" = false;
        "extensions.htmlaboutaddons.recommendations.enabled" = false;
        "browser.shell.checkDefaultBrowser" = false;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
        "browser.preferences.moreFromMozilla" = false;
        "browser.tabs.tabmanager.enabled" = false;
        "browser.aboutwelcome.enabled" = false;
        "findbar.highlightAll" = true;
        "middlemouse.contentLoadURL" = false;
        "browser.privatebrowsing.enable-new-indicator" = false;

        # Fullscreen
        "full-screen-api.transition-duration.enter" = "0 0";
        "full-screen-api.transition-duration.leave" = "0 0";
        "full-screen-api.warning.delay" = 0;
        "full-screen-api.warning.timeout" = 0;

        # URL Bar
        "browser.urlbar.suggest.engines" = false;
        "browser.urlbar.suggest.topsites" = false;
        "browser.urlbar.suggest.calculator" = true;
        "browser.urlbar.unitConversion.enabled" = true;

        # New tab page
        "browser.newtabpage.activity-stream.feeds.topsites" = false;
        "browser.newtabpage.activity-stream.feeds.section.topstories" = false;

        # Pocket
        "extensions.pocket.enabled" = false;

        # Downloads
        "browser.download.useDownloadDir" = false;
        "browser.download.alwaysOpenPanel" = false;
        "browser.download.manager.addToRecentDocs" = false;
        "browser.download.always_ask_before_handling_new_types" = true;

        # PDF
        "browser.download.open_pdf_attachments_inline" = true;

        # Tab behavior
        "browser.link.open_newwindow.restriction" = 0;
        "dom.disable_window_move_resize" = true;
        "browser.tabs.loadBookmarksInTabs" = true;
        "browser.bookmarks.openInTabClosesMenu" = false;
        "dom.popup_allowed_events" = "change click dblclick auxclick mousedown mouseup pointerdown pointerup notificationclick reset submit touchend contextmenu"; # reset pref; remove in v.111
        "layout.css.has-selector.enabled" = true;

        ################# SMOOTHFOX #################
        "general.smoothScroll" = true;
        "mousewheel.default.delta_multiplier_y" = 275;
        "general.autoScroll" = true;
        
        ################# OVERRIDES #################
        "browser.startup.homepage" = "";
        # Enable HTTPS-Only Mode
        "dom.security.https_only_mode" = true;
        "dom.security.https_only_mode_ever_enabled" = true;
        # Privacy settings
        "privacy.donottrackheader.enabled" = true;
        "privacy.trackingprotection.enabled" = true;
        "privacy.trackingprotection.socialtracking.enabled" = true;
        "privacy.partition.network_state.ocsp_cache" = true;
        # Disable all sorts of telemetry
        "toolkit.telemetry.hybridContent.enabled" = false;
        "toolkit.telemetry.reportingpolicy.firstRun" = false;

        # As well as Firefox 'experiments'
        "experiments.activeExperiment" = false;
        "experiments.enabled" = false;
        "experiments.supported" = false;
        "network.allow-experiments" = false;
        # Disable Pocket Integration
        "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
        "extensions.pocket.api" = "";
        "extensions.pocket.oAuthConsumerKey" = "";
        "extensions.pocket.showHome" = false;
        "extensions.pocket.site" = "";
        # Allow copy to clipboard
        "dom.events.asyncClipboard.clipboardItem" = true;
      };
    };
    
    package = pkgs.wrapFirefox pkgs.firefox-devedition-bin-unwrapped {
      cfg.enableTridactylNative = true;
      extraPolicies = {
        DisableAppUpdate = true;
        DisableFirefoxStudies = true;
        DisablePocket = true;
        DontCheckDefaultBrowser = true;
        PictureInPicture = {
          Enabled = false;
          Locked = true;
        };
        RequestedLocales = ["en"];
        UserMessaging = {
          ExtensionRecommendations = false;
          FeatureRecommendations = true;
          UrlbarInterventions = true;
          SkipOnboarding = true;
        };

        
        # https://github.com/mozilla/policy-templates/blob/master/README.md#extensionsettings
        # about:support
        ExtensionSettings = {
          "uBlock0@raymondhill.net" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi";
          };
          "tridactyl.vim.betas.nonewtab@cmcaine.co.uk" = {
            installation_mode = "normal_installed";
            install_url = "https://tridactyl.cmcaine.co.uk/betas/nonewtab/tridactyl_no_new_tab_beta-latest.xpi";
            install_sources = [
              "https://tridactyl.cmcaine.co.uk/betas/*"
            ];
          };
          "queryamoid@kaply.com" = {
            installation_mode = "normal_installed";
            install_url = "https://github.com/mkaply/queryamoid/releases/download/v0.2/query_amo_addon_id-0.2-fx.xpi";
            install_sources = [
              "https://github.com/mkaply/queryamoid/releases/download/*"
            ];
          };
          "{446900e4-71c2-419f-a6a7-df9c091e268b}" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/bitwarden-password-manager/latest.xpi";
          };
          "{d634138d-c276-4fc8-924b-40a0ea21d284}" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/1password-x-password-manager/latest.xpi";
          };
          "{97d566da-42c5-4ef4-a03b-5a2e5f7cbcb2}" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/awesome-rss/latest.xpi";
          };
          "clearcache@michel.de.almeida" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/clearcache/latest.xpi";
          };
          "addon@darkreader.org" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/darkreader/latest.xpi";
          };
          "enhancerforyoutube@maximerf.addons.mozilla.org" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/enhancer-for-youtube/latest.xpi";
          };
          "@contain-facebook" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/facebook-container/latest.xpi";
          };
          "@testpilot-containers" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/multi-account-containers/latest.xpi";
          };
          "FirefoxColor@mozilla.com" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/firefox-color/latest.xpi";
          };
          "87677a2c52b84ad3a151a4a72f5bd3c4@jetpack" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/grammarly-1/latest.xpi";
          };
          "linkgopher@oooninja.com" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/link-gopher/latest.xpi";
          };
          "jid1-Om7eJGwA1U8Akg@jetpack" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/octotree/latest.xpi";
          };
          "extension@one-tab.com" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/onetab/latest.xpi";
          };
          "jid0-adyhmvsP91nUO8pRv0Mn2VKeB84@jetpack" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/raindropio/latest.xpi";
          };
          "{0109ab33-601c-417c-844b-e73a1a09ea14}" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/readwise-export/latest.xpi";
          };
          "team@readwise.io" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/readwise-highlighter/latest.xpi";
          };
          "{3c078156-979c-498b-8990-85f7987dd929}" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/sidebery/latest.xpi";
          };
          "{036a55b4-5e72-4d05-a06c-cba2dfcc134a}" = {
            installation_mode = "normal_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/traduzir-paginas-web/latest.xpi";
          };
        };
      };
    };
  };

  # https://github.com/NixOS/nixpkgs/issues/47340#issuecomment-440645870
  home.file = {
    ".mozilla/native-messaging-hosts/tridactyl.json" = {
      source = "${pkgs.tridactyl-native}/lib/mozilla/native-messaging-hosts/tridactyl.json";
    };
    # NOTE it might cause a collision, just delete the file for now!
    ".mozilla/firefox/p8klfsds.dev-edition-default/containers.json" = {
      source = ./. + "/firefox/containers.json";
    };
    
  };

  xdg.configFile.tridactyl_config = {
    source = ./firefox/tridactylrc;
    target = "tridactyl/tridactylrc";
  };
}
