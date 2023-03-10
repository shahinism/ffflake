import subprocess

from libqtile import hook

from libqtile.log_utils import logger

from utils import AUTOSTART_APPS


@hook.subscribe.startup_once
def autostart():
    """
    Executed when Qtile starts
    """
    for app_cmd in AUTOSTART_APPS:
        try:
            subprocess.Popen(app_cmd, shell=True)
        except subprocess.CalledProcessError:
            logger.exception('Error while autostarting "%s" command', app_cmd)
            
# Reload config on screen changes
@hook.subscribe.screens_reconfigured
async def outputs_changed():
    logger.warning("Screens reconfigured")
    await asyncio.sleep(1)
    logger.warning("Reloading config...")
    qtile.reload_config()
