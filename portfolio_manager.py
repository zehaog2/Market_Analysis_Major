import json
import os
from stock_info_manager import StockInfoManager

class PortfolioManager:
    def __init__(self, config_file='config.json'):
        # Always use config.json from current directory (Stock_corr folder)
        self.config_file = os.path.join(os.path.dirname(__file__), config_file)
        self.config = self.load_config()
        self.info_manager = StockInfoManager()
    
    def load_config(self):
        """Load configuration file"""
        if os.path.exists(self.config_file):
            with open(self.config_file, 'r') as f:
                return json.load(f)
        else:
            # Create default config
            default_config = {
                'portfolio': {
                    'stocks': []
                }
            }
            # Save the default config
            with open(self.config_file, 'w') as f:
                json.dump(default_config, f, indent=2)
            print(f"✅ Created new config file at {self.config_file}")
            return default_config
    
    def save_config(self):
        """Save configuration file"""
        with open(self.config_file, 'w') as f:
            json.dump(self.config, f, indent=2)
        print(f"✅ Configuration saved to {self.config_file}")
    
    def add_ticker(self, ticker):
        """Add a ticker to the portfolio"""
        ticker = ticker.upper()
        if ticker not in self.config['portfolio']['stocks']:
            # Update stock info
            info = self.info_manager.update_ticker(ticker)
            
            # Add to portfolio
            self.config['portfolio']['stocks'].append(ticker)
            self.save_config()
            
            print(f"\n✅ Added {ticker} to portfolio")
            print(f"   Company: {info['company']}")
            print(f"   Industry: {info['industry']}")
            print(f"   Sector: {info['sector']}")
            print(f"   Peers: {', '.join(info['peers'])}")
            print(f"   ETF: {info['sector_etf']}")
        else:
            print(f"❌ {ticker} is already in portfolio")
    
    def remove_ticker(self, ticker):
        """Remove a ticker from the portfolio"""
        ticker = ticker.upper()
        if ticker in self.config['portfolio']['stocks']:
            self.config['portfolio']['stocks'].remove(ticker)
            self.save_config()
            print(f"✅ Removed {ticker} from portfolio")
        else:
            print(f"❌ {ticker} not found in portfolio")
    
    def list_portfolio(self):
        """List all tickers in portfolio with details"""
        stocks = self.config['portfolio']['stocks']
        if not stocks:
            print("🔭 Portfolio is empty")
            return
        
        print("\n📊 Current Portfolio:")
        print("-" * 80)
        
        for ticker in stocks:
            info = self.info_manager.get_info(ticker)
            print(f"\n{ticker}: {info['company']}")
            print(f"  Industry: {info['industry']}")
            print(f"  Sector: {info['sector']}")
            print(f"  Peers: {', '.join(info['peers'][:5])}")
            print(f"  ETF: {info['sector_etf']}")
    
    def update_all(self):
        """Force update all stock information"""
        stocks = self.config['portfolio']['stocks']
        if not stocks:
            print("🔭 Portfolio is empty")
            return
        
        print(f"\n🔄 Updating {len(stocks)} stocks...")
        self.info_manager.update_portfolio(stocks, force_update=True)
        print("✅ All stock information updated")
    
    def interactive_menu(self):
        """Interactive menu for portfolio management"""
        while True:
            print("\n" + "="*50)
            print("📈 PORTFOLIO MANAGER")
            print("="*50)
            print("1. List portfolio")
            print("2. Add ticker")
            print("3. Remove ticker")
            print("4. Update all stock info")
            print("5. Export stock info")
            print("6. Exit")
            
            choice = input("\nSelect option (1-6): ").strip()
            
            if choice == '1':
                self.list_portfolio()
            
            elif choice == '2':
                ticker = input("Enter ticker to add: ").strip().upper()
                if ticker:
                    self.add_ticker(ticker)
            
            elif choice == '3':
                ticker = input("Enter ticker to remove: ").strip().upper()
                if ticker:
                    self.remove_ticker(ticker)
            
            elif choice == '4':
                self.update_all()
            
            elif choice == '5':
                self.info_manager.export_readable()
                print("✅ Exported to stock_info_readable.json")
            
            elif choice == '6':
                print("👋 Goodbye!")
                break
            
            else:
                print("❌ Invalid option")


def main():
    """Main function with command line support"""
    import sys
    
    manager = PortfolioManager()
    
    if len(sys.argv) > 1:
        command = sys.argv[1].lower()
        
        if command == 'add' and len(sys.argv) > 2:
            for ticker in sys.argv[2:]:
                manager.add_ticker(ticker)
        
        elif command == 'remove' and len(sys.argv) > 2:
            for ticker in sys.argv[2:]:
                manager.remove_ticker(ticker)
        
        elif command == 'list':
            manager.list_portfolio()
        
        elif command == 'update':
            manager.update_all()
        
        else:
            print("Usage:")
            print("  python portfolio_manager.py          # Interactive menu")
            print("  python portfolio_manager.py add AAPL MSFT")
            print("  python portfolio_manager.py remove GME")
            print("  python portfolio_manager.py list")
            print("  python portfolio_manager.py update")
    else:
        # Interactive mode
        manager.interactive_menu()


if __name__ == "__main__":
    main()