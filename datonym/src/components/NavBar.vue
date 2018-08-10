<template>
  <nav class="navbar is-white has-shadow is-spaced">
    <div class="navbar-brand">
      <router-link class="navbar-item" to="/">Datonym</router-link>
    </div>

    <div class="navbar-menu">
      <div class="navbar-end">
        <div class="search-bar navbar-item">
          <div class="field has-addons is-marginless">
            <div class="control is-expanded">
              <input class="input" type="text"
                placeholder="Search for a name..."
                @keyup.enter="search"
                v-model="searchText">
            </div>
            <div class="control">
              <router-link class="button is-primary" :to="searchDestination">
                Search
              </router-link>
            </div>
          </div>
        </div>

        <router-link class="navbar-item" to="/about">About</router-link>
      </div>
    </div>
  </nav>
</template>

<script>
  export default {
    name: 'NavBar',
    data() {
      return {
        searchText: '',
      };
    },
    methods: {
      search() {
        this.$router.push(this.searchDestination);
      },
    },
    computed: {
      searchDestination() {
        return `/name/${this.searchText}`;
      },
    },
    watch: {
      $route(to) {
        if(to.name === 'name' && to.params && to.params.name !== this.searchText) {
          this.searchText = to.params.name;
        }
      },
    },
  };
</script>

<style scoped lang="scss">
</style>
