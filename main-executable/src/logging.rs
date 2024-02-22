use color_eyre::eyre;

pub fn setup_logger() -> eyre::Result<()>
{
    {
        use fern::colors::{Color, ColoredLevelConfig};
        use log::LevelFilter;

        let colors = ColoredLevelConfig::new()
            .info(Color::Green)
            .warn(Color::BrightYellow)
            .error(Color::BrightRed)
            .debug(Color::BrightMagenta)
            .trace(Color::BrightCyan);

        fern::Dispatch::new()
            .format(move |out, message, record| {
                out.finish(format_args!(
                    "[{} @ {}] {}",
                    colors.color(record.level()),
                    record
                        .file()
                        .map(|file| format!("{file}:{}", record.line().unwrap()))
                        .unwrap_or_else(|| record.target().into()),
                    message,
                ))
            })
            .level(LevelFilter::Trace)
            .chain(std::io::stdout())
            .apply()?;
    }

    Ok(())
}
