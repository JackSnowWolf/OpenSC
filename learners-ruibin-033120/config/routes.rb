Rails.application.routes.draw do
  # root 'test#new'
  root 'subjects#index'

  resources :users do
    resources :notes
  end

  resources :sessions, only: [:new, :create, :destroy]
  
  resources :subjects do
    resources :notes
  end

  get 'signup', to: 'users#new', as: 'signup'
  post '/users' => 'users#create'

  get 'login', to: 'sessions#new', as: 'login'
  post '/login' => 'sessions#create'

  get 'logout', to: 'sessions#destroy', as: 'logout'

  # For details on the DSL available within this file, see https://guides.rubyonrails.org/routing.html
end
